use std::{
    fmt::Display,
    iter::Sum,
    num::{IntErrorKind, ParseIntError},
    ops::Add,
    str::FromStr,
};

use rand::Rng;

#[derive(Clone)]
pub struct DiceRoll {
    dice: Vec<RollPart>,
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
                total: *bonus,
                label: None,
            },
        }
    }

    fn critical(&self) -> RollResult {
        match self {
            Self::Die(die) => die.critical(),
            Self::Bonus(_, _) => RollResult {
                specifier: vec![],
                rolls: vec![],
                crit: 0,
                total: 0,
                label: None,
            },
        }
    }
}

#[derive(Debug)]
pub enum DiceParseError {
    NoDFound(String),
    InvalidNumberOfDice(String, ParseIntError),
    TooManyDice(String),
    InvalidNumberOfFaces(String, ParseIntError),
    TooManyFaces(String),
    MalformedModifiers(String),
    InvalidArgument(String, ParseIntError),
    IllegalExplodeArgument(String, usize),
    IllegalDropArgument(String, DropRule),
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

        let input = input.replace('-', "+-");
        let input = input.trim_matches(' ').strip_prefix('+').unwrap_or(&input);

        let trimmed = input.strip_suffix('!');
        let critical = trimmed.is_some();
        let input = trimmed.unwrap_or(input);

        let dice = input
            .split('+')
            .map(|token| {
                let (token, label) = extract_label(token)?;
                let token = token.to_lowercase().replace(' ', "");
                if let Ok(bonus) = token.parse::<isize>() {
                    Ok(RollPart::Bonus(bonus, label.map(str::to_string)))
                } else {
                    Ok(RollPart::Die(token.parse::<Die>()?.with_label(label)))
                }
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
        let dice_total = self.dice.iter().map(RollPart::roll).sum::<RollResult>();

        let dice_total = if self.critical {
            let critical = self.dice.iter().map(RollPart::critical).sum::<RollResult>();
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

    /// Reroll dice under this threshold (once)
    reroll: usize,

    /// Explode dice above or equal to this threshold (repeatedly)
    explode: usize,

    /// Drop lowest or highest rolls
    drop: Option<DropRule>,

    /// Count successes above a this threshold
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

        let flags = &['k', 'd', 'r', 'e', 'c'];
        let mut tokens = die_specifier.split_inclusive(flags).collect::<Vec<_>>();
        if tokens[tokens.len() - 1].ends_with(flags) {
            tokens.push("");
        }
        let tokens = tokens;

        let faces_str = tokens[0].strip_suffix(flags).unwrap_or(tokens[0]);

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

        if faces > 1048576 {
            return Err(DiceParseError::TooManyFaces(s.to_string()));
        }

        let mut reroll = 0;
        let mut explode = faces + 1;
        let mut drop = None;
        let mut count = None;

        for window in tokens.windows(2) {
            let sep = window[0]
                .chars()
                .next_back()
                .ok_or_else(|| DiceParseError::MalformedModifiers(s.to_string()))?;
            let args = window[1].strip_suffix(flags).unwrap_or(window[1]);
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
                    drop = Some(
                        droprule
                            .map_err(|err| DiceParseError::InvalidArgument(s.to_string(), err))?,
                    );
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
                    drop = Some(
                        droprule
                            .map_err(|err| DiceParseError::InvalidArgument(s.to_string(), err))?,
                    );
                }
                'r' => {
                    reroll = if args.is_empty() {
                        1
                    } else {
                        args.parse()
                            .map_err(|err| DiceParseError::InvalidArgument(s.to_string(), err))?
                    };
                }
                'e' => {
                    explode = if args.is_empty() {
                        faces
                    } else {
                        args.parse()
                            .map_err(|err| DiceParseError::InvalidArgument(s.to_string(), err))?
                    };
                }
                'c' => {
                    count = Some(if args.is_empty() {
                        faces
                    } else {
                        args.parse()
                            .map_err(|err| DiceParseError::InvalidArgument(s.to_string(), err))?
                    });
                }
                _ => {}
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
            explode,
            drop,
            count,
            negative: num < 0,
        })
    }
}

pub struct RollResult {
    pub specifier: Vec<String>,
    pub rolls: Vec<Vec<Roll>>,
    pub crit: isize,
    pub total: isize,
    pub label: Option<String>,
}

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
            .map(|_| Roll::random(self.faces))
            .collect::<Vec<_>>();

        let n_reroll = rolls
            .iter_mut()
            .filter(|roll| roll.result as usize <= self.reroll)
            .map(|roll| {
                roll.dropped = true;
            })
            .count();
        if n_reroll > 0 {
            rolls.extend(
                (0..n_reroll)
                    .map(|_| Roll::random(self.faces))
                    .collect::<Vec<_>>(),
            );
        }

        let mut n_explode = rolls
            .iter_mut()
            .filter(|roll| !roll.dropped && (roll.result as usize) >= self.explode)
            .map(|roll| {
                roll.exploded = true;
            })
            .count();
        while n_explode > 0 {
            let mut new_rolls = (0..n_explode)
                .map(|_| Roll::random(self.faces))
                .collect::<Vec<_>>();
            n_explode = new_rolls
                .iter_mut()
                .filter(|roll| !roll.dropped && (roll.result as usize) >= self.explode)
                .map(|roll| {
                    roll.exploded = true;
                })
                .count();
            rolls.extend(new_rolls);
        }

        if let Some(drop) = &self.drop {
            let mut indices = (0..rolls.len())
                .filter(|&idx| !rolls[idx].dropped)
                .collect::<Vec<_>>();
            indices.sort_by_key(|&idx| &rolls[idx].result);
            let to_drop = match drop {
                DropRule::KeepLowest(n_keep) => &indices[*n_keep..],
                DropRule::KeepHighest(n_keep) => &indices[..(indices.len() - n_keep)],
                DropRule::DropLowest(n_keep) => &indices[..*n_keep],
                DropRule::DropHighest(n_keep) => &indices[(indices.len() - n_keep)..],
            };
            to_drop.iter().for_each(|&idx| rolls[idx].dropped = true);
        }

        let mut total = match self.count {
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

        if self.negative {
            rolls = rolls
                .iter()
                .map(|roll| Roll {
                    result: -roll.result,
                    ..*roll
                })
                .collect();
            total = -total;
        }

        RollResult {
            specifier: vec![self.specifier.clone()],
            rolls: vec![rolls],
            crit: 0,
            total,
            label: None,
        }
    }

    pub fn critical(&self) -> RollResult {
        if self.negative {
            return RollResult {
                specifier: vec![],
                rolls: vec![],
                crit: 0,
                total: 0,
                label: None,
            };
        }

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

        RollResult {
            specifier: vec![],
            rolls: vec![],
            crit: total,
            total,
            label: None,
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
        let rolls = self.specifier.join(" + ").replace(" + -", " - ");
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
            .join(" + ")
            .replace(" + -", " - ");
        let rolls = if self.crit != 0 {
            format!("{} + {}!", rolls, self.crit)
        } else {
            rolls
        };
        let label = self
            .label
            .as_ref()
            .map(|lbl| format!("*{}:* ", lbl))
            .unwrap_or_else(|| "".to_string());
        write!(f, "{}{} = **{}**", label, rolls, self.total)
    }
}

impl Sum<RollResult> for RollResult {
    fn sum<I: Iterator<Item = RollResult>>(iter: I) -> Self {
        iter.fold(
            RollResult {
                specifier: vec![],
                rolls: vec![],
                crit: 0,
                total: 0,
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
            ..self
        }
    }
}

impl Add<RollResult> for RollResult {
    type Output = RollResult;

    fn add(self, rhs: RollResult) -> Self::Output {
        let mut rolls = self.rolls;
        rolls.extend(rhs.rolls.into_iter());
        let mut specifier = self.specifier;
        specifier.extend(rhs.specifier.into_iter());
        RollResult {
            specifier,
            rolls,
            crit: self.crit + rhs.crit,
            total: self.total + rhs.total,
            label: None,
        }
    }
}
