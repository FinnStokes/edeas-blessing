use std::{
    fmt::Display,
    iter::{self, Product, Sum},
    num::{IntErrorKind, ParseIntError},
    ops::{Add, Mul},
    str::FromStr,
};

use rand::Rng;
use thiserror::Error;

#[derive(Clone)]
pub struct DiceRoll {
    dice: Vec<Vec<RollPart>>,
    critical: bool,
    label: Option<String>,
}

#[derive(Clone)]
enum RollPart {
    Die(Die),
    Bonus(isize, Option<String>),
}

impl RollPart {
    fn roll(&self) -> RollResult {
        match self {
            Self::Die(die) => die.roll(),
            Self::Bonus(bonus, label) => RollResult {
                specifier: vec![if let Some(label) = label {
                    format!("{} *[{}]*", bonus, label)
                } else {
                    bonus.to_string()
                }],
                rolls: vec![vec![]],
                crit: 0,
                base: *bonus,
                total: *bonus,
                fails: 0,
                label: None,
            },
        }
    }

    fn critical(&self) -> RollResult {
        match self {
            Self::Die(die) => die.critical(),
            Self::Bonus(bonus, _) => RollResult {
                specifier: vec![],
                rolls: vec![],
                crit: 0,
                base: *bonus,
                total: 0,
                fails: 0,
                label: None,
            },
        }
    }
}

#[derive(Error, Debug)]
pub enum DiceParseError {
    #[error("invalid dice string `{0}`: no 'd' separator")]
    NoDFound(String),

    #[error("invalid dice string `{0}`: malformed number of dice")]
    InvalidNumberOfDice(String, ParseIntError),

    #[error("invalid dice string `{0}`: too many dice (max 1024)")]
    TooManyDice(String),

    #[error("invalid dice string `{0}`: malformed number of faces")]
    InvalidNumberOfFaces(String, ParseIntError),

    #[error("invalid dice string `{0}`: not enough faces (min 1)")]
    NotEnoughFaces(String),

    #[error("invalid dice string `{0}`: too many faces (max 1048576)")]
    TooManyFaces(String),

    #[error("invalid dice string `{0}`: unknown or malformed modifiers")]
    MalformedModifiers(String),

    #[error("invalid dice string `{0}`: malformed argument for modifier")]
    InvalidArgument(String, ParseIntError),

    #[error("invalid dice string `{0}`: illegal explode argument (min 2)")]
    IllegalExplodeArgument(String, usize),

    #[error("invalid dice string `{0}`: illegal drop argument (max n_dice-1)")]
    IllegalDropArgument(String, DropRule),

    #[error("invalid dice string `{0}`: malformed label")]
    InvalidLabel(String),
}

fn extract_label(input: &str) -> Result<(&str, Option<&str>), DiceParseError> {
    let parts = input
        .trim_end()
        .split_inclusive(&['[', ']'])
        .collect::<Vec<_>>();

    match &parts[..] {
        [token] => Ok((token, None)),
        [token, label] | [token, label, ""] => {
            let token = token
                .strip_suffix('[')
                .ok_or_else(|| DiceParseError::InvalidLabel(input.to_string()))?;

            let label = label
                .strip_suffix(']')
                .ok_or_else(|| DiceParseError::InvalidLabel(input.to_string()))?;

            Ok((token, Some(label)))
        }
        _ => Err(DiceParseError::InvalidLabel(input.to_string())),
    }
}

impl FromStr for DiceRoll {
    type Err = DiceParseError;

    fn from_str(input: &str) -> Result<Self, Self::Err> {
        let (label, input) = input
            .split_once(':')
            .map(|(label, input)| (Some(label), input))
            .unwrap_or((None, input));

        // Allow x, *, or unicode multiplication symbol for multiplication
        let input = input.replace(&['×', 'x'], "*");

        // Shenanigans to turn subtraction into addition of a negative without breaking
        // negatives in leading roll or multiplications

        // Convert hyphen to minus sign to mark as unprocessed
        let input = input.replace('-', "−");
        // Convert leading minus sign or minus sign following multiplication to hyphen
        let input = input
            .split('*')
            .map(|s| {
                s.trim()
                    .strip_prefix('−')
                    .map(|suffix| format!("-{}", suffix))
                    .unwrap_or(s.trim().to_owned())
            })
            .collect::<Vec<_>>()
            .join("*");
        // Insert + before every remaining minus sign
        let input = input.replace('−', "+-");

        // Trim leading or trailing whitespace
        let input = input.trim();

        // Parse trailing exclamation point as critical flag
        let trimmed = input.strip_suffix('!');
        let critical = trimmed.is_some();
        let input = trimmed.unwrap_or(input);

        let dice = input
            .split('+')
            .map(|product| {
                product
                    .split('*')
                    .map(|token| {
                        let (token, label) = extract_label(token)?;
                        let token = token.to_lowercase().replace(' ', "");
                        if let Ok(bonus) = token.parse::<isize>() {
                            Ok(RollPart::Bonus(bonus, label.map(str::to_string)))
                        } else {
                            Ok(RollPart::Die(token.parse::<Die>()?.with_label(label)))
                        }
                    })
                    .collect::<Result<_, DiceParseError>>()
            })
            .collect::<Result<_, DiceParseError>>()?;

        Ok(Self {
            dice,
            critical,
            label: label.map(|lbl| lbl.to_string()),
        })
    }
}

impl DiceRoll {
    pub fn roll(&self) -> RollResult {
        let dice_total = self
            .dice
            .iter()
            .map(|product| product.iter().map(RollPart::roll).product())
            .sum::<RollResult>();

        let dice_total = if self.critical {
            let critical = self
                .dice
                .iter()
                .map(|product| product.iter().map(RollPart::critical).product())
                .sum::<RollResult>();
            dice_total + critical
        } else {
            dice_total
        };

        if let Some(label) = self.label.as_ref() {
            dice_total.with_label(label.clone())
        } else {
            dice_total
        }
    }
}

#[derive(Clone, Debug)]
pub enum DropRule {
    KeepHighest(usize),
    KeepLowest(usize),
    DropHighest(usize),
    DropLowest(usize),
}

#[derive(Clone)]
struct Die {
    /// Original command used to specify this roll
    specifier: String,

    /// Number of dice to initially roll
    num: usize,

    /// Dice should have this many faces
    faces: usize,

    /// Reroll dice under this threshold
    reroll: usize,

    /// If false, only reroll dice once, if true keep rerolling until over threshold
    chain_reroll: bool,

    /// Explode dice above or equal to this threshold (repeatedly)
    /// We don't reroll explosions, as that could lead to an infinite loop
    explode: usize,

    /// Drop lowest or highest rolls
    drop: Option<DropRule>,

    /// Count successes above this threshold
    count: Option<usize>,

    /// Subtract roll from result
    negative: bool,
}

// d6
// 3d6
// 2d20k1 - KeepHighest(1)
// 3d20kh2 - KeepHighest(2)
// 2d20kl1 = KeepLowest(1)
// 2d20dl1 = DropLowest(1)
// 2d6e6 = explode: 6
// 2d6r2 = reroll: 2
impl FromStr for Die {
    type Err = DiceParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let (num_dice, die_specifier) = s
            .split_once('d')
            .ok_or_else(|| DiceParseError::NoDFound(s.to_string()))?;

        let num: isize = if num_dice.is_empty() {
            1
        } else if num_dice == "-" {
            -1
        } else {
            num_dice.parse::<isize>().map_err(|err| {
                if err.kind() == &IntErrorKind::PosOverflow
                    || err.kind() == &IntErrorKind::NegOverflow
                {
                    DiceParseError::TooManyDice(s.to_string())
                } else {
                    DiceParseError::InvalidNumberOfDice(s.to_string(), err)
                }
            })?
        };

        if num.abs() > 1024 {
            return Err(DiceParseError::TooManyDice(s.to_string()));
        }

        let flags = &['k', 'd', 'r', 'e', 'c', '*'];
        let mut tokens = die_specifier.split_inclusive(flags).collect::<Vec<_>>();
        if let Some(token) = tokens.last() {
            if token.ends_with(flags) {
                tokens.push("");
            }
        }
        let tokens = tokens;

        let faces_str = tokens.first().unwrap_or(&"");
        let faces_str = faces_str.strip_suffix(flags).unwrap_or(faces_str);

        let faces = faces_str.parse::<usize>().map_err(|err| {
            if faces_str
                .chars()
                .take_while(|&c| char::is_numeric(c))
                .collect::<String>()
                .parse::<usize>()
                .is_err()
            {
                if err.kind() == &IntErrorKind::PosOverflow {
                    DiceParseError::TooManyFaces(s.to_string())
                } else {
                    DiceParseError::InvalidNumberOfFaces(s.to_string(), err)
                }
            } else {
                DiceParseError::MalformedModifiers(s.to_string())
            }
        })?;

        if faces < 1 {
            return Err(DiceParseError::NotEnoughFaces(s.to_string()));
        }

        if faces > 1048576 {
            return Err(DiceParseError::TooManyFaces(s.to_string()));
        }

        let mut reroll = 0;
        let mut chain_reroll = false;
        let mut explode = faces + 1;
        let mut drop = None;
        let mut count = None;

        for window in tokens.windows(2) {
            if let [prev, current] = window {
                let sep = prev
                    .chars()
                    .next_back()
                    .ok_or_else(|| DiceParseError::MalformedModifiers(s.to_string()))?;
                let args = current.strip_suffix(flags).unwrap_or(current);
                match sep {
                    'k' => {
                        let droprule = if let Some(num_keep) = args.strip_prefix('l') {
                            if num_keep.is_empty() {
                                Ok(DropRule::KeepLowest(1))
                            } else {
                                num_keep.parse().map(DropRule::KeepLowest)
                            }
                        } else if let Some(num_keep) = args.strip_prefix('h') {
                            if num_keep.is_empty() {
                                Ok(DropRule::KeepHighest(1))
                            } else {
                                num_keep.parse().map(DropRule::KeepHighest)
                            }
                        } else {
                            args.parse().map(DropRule::KeepHighest)
                        };
                        drop =
                            Some(droprule.map_err(|err| {
                                DiceParseError::InvalidArgument(s.to_string(), err)
                            })?);
                    }
                    'd' => {
                        let droprule = if let Some(num_drop) = args.strip_prefix('l') {
                            if num_drop.is_empty() {
                                Ok(DropRule::DropLowest(1))
                            } else {
                                num_drop.parse().map(DropRule::DropLowest)
                            }
                        } else if let Some(num_drop) = args.strip_prefix('h') {
                            if num_drop.is_empty() {
                                Ok(DropRule::DropHighest(1))
                            } else {
                                num_drop.parse().map(DropRule::DropHighest)
                            }
                        } else {
                            return Err(DiceParseError::MalformedModifiers(s.to_string()));
                        };
                        drop =
                            Some(droprule.map_err(|err| {
                                DiceParseError::InvalidArgument(s.to_string(), err)
                            })?);
                    }
                    'r' => {
                        chain_reroll = false;
                        reroll = if args.is_empty() {
                            1
                        } else {
                            args.parse().map_err(|err| {
                                DiceParseError::InvalidArgument(s.to_string(), err)
                            })?
                        };
                    }
                    'e' => {
                        explode = if args.is_empty() {
                            faces
                        } else {
                            args.parse().map_err(|err| {
                                DiceParseError::InvalidArgument(s.to_string(), err)
                            })?
                        };
                    }
                    'c' => {
                        count = Some(if args.is_empty() {
                            faces
                        } else {
                            args.parse().map_err(|err| {
                                DiceParseError::InvalidArgument(s.to_string(), err)
                            })?
                        });
                    }
                    _ => {}
                }
            }
        }

        if let Some(drop) = &drop {
            let drop_n = match drop {
                DropRule::KeepHighest(n) => *n,
                DropRule::KeepLowest(n) => *n,
                DropRule::DropHighest(n) => *n,
                DropRule::DropLowest(n) => *n,
            };
            if drop_n >= num.unsigned_abs() {
                return Err(DiceParseError::IllegalDropArgument(
                    s.to_string(),
                    drop.clone(),
                ));
            }
        }

        if explode <= 1 {
            return Err(DiceParseError::IllegalExplodeArgument(
                s.to_string(),
                explode,
            ));
        }

        Ok(Die {
            specifier: s.to_string(),
            num: num.unsigned_abs(),
            faces,
            reroll,
            chain_reroll,
            explode,
            drop,
            count,
            negative: num < 0,
        })
    }
}

#[derive(Debug)]
pub struct RollResult {
    pub specifier: Vec<String>,
    pub rolls: Vec<Vec<Roll>>,
    pub crit: isize,
    pub base: isize,
    pub total: isize,
    pub fails: isize,
    pub label: Option<String>,
}

#[derive(Debug)]
pub struct Roll {
    faces: usize,
    result: isize,
    dropped: bool,
    exploded: bool,
    counted: bool,
}

impl Roll {
    pub fn random(faces: usize) -> Self {
        Self {
            faces,
            result: rand::thread_rng().gen_range(1..=faces) as isize,
            dropped: false,
            exploded: false,
            counted: false,
        }
    }

    pub fn maximise(faces: usize) -> Self {
        Self {
            faces,
            result: faces as isize,
            dropped: false,
            exploded: false,
            counted: false,
        }
    }
}

impl Die {
    pub fn roll(&self) -> RollResult {
        let mut rolls = (0..self.num)
            .flat_map(|_| {
                // Reroll dice that are below reroll threshold
                iter::repeat_with(|| Roll::random(self.faces))
                    .enumerate()
                    .scan(true, |reroll, (n_roll, roll)| {
                        // Once we get a roll that we keep (i.e. do not reroll), we are done
                        if !*reroll {
                            return None;
                        }

                        // Compute reroll condition
                        *reroll = roll.result <= self.reroll as isize
                            && (n_roll == 0 || self.chain_reroll); // If not chaining rerolls, only reroll first roll
                        Some(Roll {
                            dropped: *reroll, // Mark rerolled dice as dropped
                            ..roll
                        })
                    })
            })
            .flat_map(|roll| {
                // Roll extra dice for any rolls that are above explode threshold
                iter::once(roll)
                    .chain(iter::repeat_with(|| Roll::random(self.faces)))
                    .scan(true, |explode, roll| {
                        // Once we get a roll that doesn't explode, we are done
                        if !*explode {
                            return None;
                        }

                        // Compute explode condition
                        *explode = roll.result >= self.explode as isize;
                        Some(Roll {
                            exploded: *explode, // Mark rerolled dice as dropped
                            ..roll
                        })
                    })
            })
            .collect::<Vec<_>>();

        if let Some(drop) = &self.drop {
            let mut undropped = rolls
                .iter_mut()
                .filter(|roll| !roll.dropped)
                .collect::<Vec<_>>();
            undropped.sort_by_key(|roll| roll.result);
            let n_total = undropped.len();
            let to_drop = match drop {
                DropRule::KeepLowest(n_keep) => undropped.split_at_mut(*n_keep).1,
                DropRule::KeepHighest(n_keep) => undropped.split_at_mut(n_total - n_keep).0,
                DropRule::DropLowest(n_drop) => undropped.split_at_mut(*n_drop).0,
                DropRule::DropHighest(n_drop) => undropped.split_at_mut(n_total - n_drop).1,
            };
            to_drop.iter_mut().for_each(|roll| roll.dropped = true);
        }

        let total = match self.count {
            None => rolls
                .iter()
                .filter(|roll| !roll.dropped)
                .map(|roll| roll.result as isize)
                .sum(),
            Some(threshold) => rolls
                .iter_mut()
                .filter(|roll| !roll.dropped && roll.result as usize >= threshold)
                .map(|roll| roll.counted = true)
                .count() as isize,
        };

        let fails = match self.count {
            None => 0,
            Some(_) => rolls
                .iter_mut()
                .filter(|roll| !roll.dropped && roll.result as usize == 1)
                .count() as isize,
        };

        let multiplier = if self.negative { -1 } else { 1 };

        let rolls = rolls
            .iter()
            .map(|roll| Roll {
                result: roll.result * multiplier,
                ..*roll
            })
            .collect();

        RollResult {
            specifier: vec![self.specifier.clone()],
            rolls: vec![rolls],
            crit: 0,
            base: total * multiplier,
            total: total * multiplier,
            fails: fails * multiplier,
            label: None,
        }
    }

    pub fn critical(&self) -> RollResult {
        let rolls = (0..self.num)
            .map(|_| Roll::maximise(self.faces))
            .collect::<Vec<_>>();

        let total = match self.count {
            None => rolls.iter().map(|roll| roll.result as isize).sum(),
            Some(threshold) => rolls
                .iter()
                .filter(|roll| roll.result as usize >= threshold)
                .count() as isize,
        };

        if self.negative {
            return RollResult {
                specifier: vec![],
                rolls: vec![],
                crit: 0,
                base: total,
                total: 0,
                fails: 0,
                label: None,
            };
        } else {
            RollResult {
                specifier: vec![],
                rolls: vec![],
                crit: total,
                base: total,
                total,
                fails: 0,
                label: None,
            }
        }
    }

    fn with_label(self, label: Option<&str>) -> Self {
        if let Some(label) = label {
            Self {
                specifier: format!("{} *[{}]*", self.specifier, label),
                ..self
            }
        } else {
            self
        }
    }
}

impl RollResult {
    pub fn roll(&self) -> String {
        let label = self
            .label
            .as_ref()
            .map(|lbl| format!("*{}:* ", lbl))
            .unwrap_or_else(|| "".to_string());
        let rolls = self.specifier.join("").replace(" + -", " - ");
        let crit = if self.crit != 0 { "!" } else { "" };
        format!("{}{}{}", label, rolls, crit)
    }

    pub fn with_label(self, label: String) -> Self {
        Self {
            label: Some(label),
            ..self
        }
    }
}

impl Display for RollResult {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let rolls = self
            .rolls
            .iter()
            .zip(self.specifier.iter())
            .map(|(rolls, specifier)| {
                if !rolls.is_empty() {
                    let rolls_str = rolls
                        .iter()
                        .map(|roll| {
                            if roll.dropped {
                                format!("~~{}~~", roll.result)
                            } else if roll.result.abs() >= (roll.faces as isize) || roll.counted {
                                if roll.exploded {
                                    format!("***{}***", roll.result)
                                } else {
                                    format!("**{}**", roll.result)
                                }
                            } else if roll.exploded {
                                format!("*{}*", roll.result)
                            } else {
                                roll.result.to_string()
                            }
                        })
                        .collect::<Vec<_>>()
                        .join(",");
                    format!("{} ({})", specifier, rolls_str)
                } else {
                    specifier.clone()
                }
            })
            .collect::<Vec<_>>()
            .join("")
            .replace(" + -", " − ")
            .replace('-', "−");
        let rolls = if self.crit != 0 {
            format!("{} + {}!", rolls, self.crit)
        } else {
            rolls
        };
        let fail = if self.fails > 0 && self.fails > self.total {
            " (**Critical fail!**)"
        } else {
            ""
        };
        let label = self
            .label
            .as_ref()
            .map(|lbl| format!("*{}:* ", lbl))
            .unwrap_or_else(|| "".to_string());
        write!(f, "{}{} = **{}**{}", label, rolls, self.total, fail)
    }
}

impl Sum<RollResult> for RollResult {
    fn sum<I: Iterator<Item = RollResult>>(iter: I) -> Self {
        iter.fold(
            RollResult {
                specifier: vec![],
                rolls: vec![],
                crit: 0,
                base: 0,
                total: 0,
                fails: 0,
                label: None,
            },
            RollResult::add,
        )
    }
}

impl Add<isize> for RollResult {
    type Output = RollResult;

    fn add(self, rhs: isize) -> Self::Output {
        RollResult {
            total: self.total + rhs,
            base: self.base + rhs,
            ..self
        }
    }
}

impl Add<RollResult> for RollResult {
    type Output = RollResult;

    fn add(self, rhs: RollResult) -> Self::Output {
        let mut rolls = self.rolls;
        rolls.extend(rhs.rolls.into_iter());
        let specifier = if self.specifier.len() > 0 {
            let mut specifier = self.specifier;
            specifier.extend(
                rhs.specifier
                    .into_iter()
                    .enumerate()
                    .map(|(idx, specifier)| {
                        if idx == 0 {
                            format!(" + {}", specifier)
                        } else {
                            specifier
                        }
                    }),
            );
            specifier
        } else {
            rhs.specifier
        };
        RollResult {
            specifier,
            rolls,
            crit: self.crit + rhs.crit,
            base: self.base + rhs.base,
            total: self.total + rhs.total,
            fails: self.fails + rhs.fails,
            label: None,
        }
    }
}

impl Product<RollResult> for RollResult {
    fn product<I: Iterator<Item = RollResult>>(iter: I) -> Self {
        iter.fold(
            RollResult {
                specifier: vec![],
                rolls: vec![],
                crit: 0,
                base: 1,
                total: 0,
                fails: 0,
                label: None,
            },
            RollResult::mul,
        )
    }
}

impl Mul<isize> for RollResult {
    type Output = RollResult;

    fn mul(self, rhs: isize) -> Self::Output {
        RollResult {
            total: self.total * rhs,
            crit: self.crit * rhs,
            base: self.base * rhs,
            fails: self.fails * rhs,
            ..self
        }
    }
}

impl Mul<RollResult> for RollResult {
    type Output = RollResult;

    fn mul(self, rhs: RollResult) -> Self::Output {
        let mut rolls = self.rolls;
        rolls.extend(rhs.rolls.into_iter());
        let specifier = if self.specifier.len() > 0 {
            let mut specifier = self.specifier;
            specifier.extend(
                rhs.specifier
                    .into_iter()
                    .enumerate()
                    .map(|(idx, specifier)| {
                        if idx == 0 {
                            format!(" × {}", specifier)
                        } else {
                            specifier
                        }
                    }),
            );
            specifier
        } else {
            rhs.specifier
        };

        let total = self.base * rhs.total + self.total * rhs.base - self.total * rhs.total;
        let crit = self.base * rhs.crit + self.crit * rhs.base - self.crit * rhs.crit;
        let fails = self.base * rhs.fails + self.fails * rhs.base;

        RollResult {
            specifier,
            rolls,
            total: if crit > 0 { total } else { total - crit },
            crit: if crit > 0 { crit } else { 0 },
            fails,
            base: self.base * rhs.base,
            label: None,
        }
    }
}
