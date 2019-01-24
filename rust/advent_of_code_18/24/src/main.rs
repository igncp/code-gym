/*

--- Day 24: Immune System Simulator 20XX ---

After a weird buzzing noise, you appear back at the man's cottage. He seems relieved to see his
friend, but quickly notices that the little reindeer caught some kind of cold while out exploring.

The portly man explains that this reindeer's immune system isn't similar to regular reindeer immune
systems:

The immune system and the infection each have an army made up of several groups; each group
consists of one or more identical units. The armies repeatedly fight until only one army has units
remaining.

Units within a group all have the same hit points (amount of damage a unit can take before it is
destroyed), attack damage (the amount of damage each unit deals), an attack type, an initiative
(higher initiative units attack first and win ties), and sometimes weaknesses or immunities. Here
is an example group:

18 units each with 729 hit points (weak to fire; immune to cold, slashing)
 with an attack that does 8 radiation damage at initiative 10

Each group also has an effective power: the number of units in that group multiplied by their
attack damage. The above group has an effective power of 18 * 8 = 144. Groups never have zero or
negative units; instead, the group is removed from combat.

Each fight consists of two phases: target selection and attacking.

During the target selection phase, each group attempts to choose one target. In decreasing order of
effective power, groups choose their targets; in a tie, the group with the higher initiative
chooses first. The attacking group chooses to target the group in the enemy army to which it would
deal the most damage (after accounting for weaknesses and immunities, but not accounting for
whether the defending group has enough units to actually receive all of that damage).

If an attacking group is considering two defending groups to which it would deal equal damage, it
chooses to target the defending group with the largest effective power; if there is still a tie, it
chooses the defending group with the highest initiative. If it cannot deal any defending groups
damage, it does not choose a target. Defending groups can only be chosen as a target by one
attacking group.

At the end of the target selection phase, each group has selected zero or one groups to attack, and
each group is being attacked by zero or one groups.

During the attacking phase, each group deals damage to the target it selected, if any. Groups
attack in decreasing order of initiative, regardless of whether they are part of the infection or
the immune system. (If a group contains no units, it cannot attack.)

The damage an attacking group deals to a defending group depends on the attacking group's attack
type and the defending group's immunities and weaknesses. By default, an attacking group would deal
damage equal to its effective power to the defending group. However, if the defending group is
immune to the attacking group's attack type, the defending group instead takes no damage; if the
defending group is weak to the attacking group's attack type, the defending group instead takes
double damage.

The defending group only loses whole units from damage; damage is always dealt in such a way that
it kills the most units possible, and any remaining damage to a unit that does not immediately kill
it is ignored. For example, if a defending group contains 10 units with 10 hit points each and
receives 75 damage, it loses exactly 7 units and is left with 3 units at full health.

After the fight is over, if both armies still contain units, a new fight begins; combat only ends
once one army has lost all of its units.

For example, consider the following armies:

Immune System:
17 units each with 5390 hit points (weak to radiation, bludgeoning) with
 an attack that does 4507 fire damage at initiative 2
989 units each with 1274 hit points (immune to fire; weak to bludgeoning,
 slashing) with an attack that does 25 slashing damage at initiative 3

Infection:
801 units each with 4706 hit points (weak to radiation) with an attack
 that does 116 bludgeoning damage at initiative 1
4485 units each with 2961 hit points (immune to radiation; weak to fire,
 cold) with an attack that does 12 slashing damage at initiative 4

If these armies were to enter combat, the following fights, including details during the target
selection and attacking phases, would take place:

Immune System:
Group 1 contains 17 units
Group 2 contains 989 units
Infection:
Group 1 contains 801 units
Group 2 contains 4485 units

Infection group 1 would deal defending group 1 185832 damage
Infection group 1 would deal defending group 2 185832 damage
Infection group 2 would deal defending group 2 107640 damage
Immune System group 1 would deal defending group 1 76619 damage
Immune System group 1 would deal defending group 2 153238 damage
Immune System group 2 would deal defending group 1 24725 damage

Infection group 2 attacks defending group 2, killing 84 units
Immune System group 2 attacks defending group 1, killing 4 units
Immune System group 1 attacks defending group 2, killing 51 units
Infection group 1 attacks defending group 1, killing 17 units

Immune System:
Group 2 contains 905 units
Infection:
Group 1 contains 797 units
Group 2 contains 4434 units

Infection group 1 would deal defending group 2 184904 damage
Immune System group 2 would deal defending group 1 22625 damage
Immune System group 2 would deal defending group 2 22625 damage

Immune System group 2 attacks defending group 1, killing 4 units
Infection group 1 attacks defending group 2, killing 144 units
Immune System:
Group 2 contains 761 units
Infection:
Group 1 contains 793 units
Group 2 contains 4434 units

Infection group 1 would deal defending group 2 183976 damage
Immune System group 2 would deal defending group 1 19025 damage
Immune System group 2 would deal defending group 2 19025 damage

Immune System group 2 attacks defending group 1, killing 4 units
Infection group 1 attacks defending group 2, killing 143 units
Immune System:
Group 2 contains 618 units
Infection:
Group 1 contains 789 units
Group 2 contains 4434 units

Infection group 1 would deal defending group 2 183048 damage
Immune System group 2 would deal defending group 1 15450 damage
Immune System group 2 would deal defending group 2 15450 damage

Immune System group 2 attacks defending group 1, killing 3 units
Infection group 1 attacks defending group 2, killing 143 units
Immune System:
Group 2 contains 475 units
Infection:
Group 1 contains 786 units
Group 2 contains 4434 units

Infection group 1 would deal defending group 2 182352 damage
Immune System group 2 would deal defending group 1 11875 damage
Immune System group 2 would deal defending group 2 11875 damage

Immune System group 2 attacks defending group 1, killing 2 units
Infection group 1 attacks defending group 2, killing 142 units
Immune System:
Group 2 contains 333 units
Infection:
Group 1 contains 784 units
Group 2 contains 4434 units

Infection group 1 would deal defending group 2 181888 damage
Immune System group 2 would deal defending group 1 8325 damage
Immune System group 2 would deal defending group 2 8325 damage

Immune System group 2 attacks defending group 1, killing 1 unit
Infection group 1 attacks defending group 2, killing 142 units
Immune System:
Group 2 contains 191 units
Infection:
Group 1 contains 783 units
Group 2 contains 4434 units

Infection group 1 would deal defending group 2 181656 damage
Immune System group 2 would deal defending group 1 4775 damage
Immune System group 2 would deal defending group 2 4775 damage

Immune System group 2 attacks defending group 1, killing 1 unit
Infection group 1 attacks defending group 2, killing 142 units
Immune System:
Group 2 contains 49 units
Infection:
Group 1 contains 782 units
Group 2 contains 4434 units

Infection group 1 would deal defending group 2 181424 damage
Immune System group 2 would deal defending group 1 1225 damage
Immune System group 2 would deal defending group 2 1225 damage

Immune System group 2 attacks defending group 1, killing 0 units
Infection group 1 attacks defending group 2, killing 49 units
Immune System:
No groups remain.
Infection:
Group 1 contains 782 units
Group 2 contains 4434 units

In the example above, the winning army ends up with 782 + 4434 = 5216 units.

You scan the reindeer's condition (your puzzle input); the white-bearded man looks nervous. As it
stands now, how many units would the winning army have?

*/

extern crate regex;

use regex::Regex;
use std::cmp::Ordering;
use std::collections::HashMap;
use std::collections::HashSet;
use std::fs::File;
use std::io::prelude::*;

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
enum GroupType {
  Infection,
  Immune,
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
enum AttackType {
  Bludgeon,
  Radiation,
  Slash,
  Cold,
  Fire,
}

type GroupId = usize;

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
struct Group {
  id: GroupId,
  units_num: usize,
  hit_points: usize,
  immune_to: Vec<AttackType>,
  weak_to: Vec<AttackType>,
  attack_damage: usize,
  attack_type: AttackType,
  group_type: GroupType,
  initiative: usize,
}

type Regs = (Regex, Regex, Regex, Regex);

impl Group {
  fn new_from_text(text: &str) -> Vec<Group> {
    let mut group_type = GroupType::Immune;
    let mut groups = vec![];
    let regs = Group::get_regs();
    let mut id = 0;

    for (idx, line) in text.lines().enumerate() {
      match line {
        "Immune System:" => {
          group_type = GroupType::Immune;
        }
        "Infection:" => {
          group_type = GroupType::Infection;
        }
        "" => {}
        _ => {
          let group = Group::new_from_line(&line, group_type, &regs, id);
          groups.push(group);
          id += 1;
        }
      }
    }

    groups
  }

  fn get_regs() -> Regs {
    let line_reg = Regex::new(r"^(.+) units each with (.+) hit points(.+)with an attack that does (.+) (.+) damage at initiative (.+)$").unwrap();
    let reg_immune = Regex::new(r"immune to ([^;)]+);?").unwrap();
    let reg_weak = Regex::new(r"weak to ([^;)]+);?").unwrap();
    let reg_word = Regex::new(r"(\w+)").unwrap();

    (line_reg, reg_immune, reg_weak, reg_word)
  }

  fn new_from_line(line: &str, group_type: GroupType, regs: &Regs, id: usize) -> Group {
    fn parse_attack_type(word: &str) -> AttackType {
      let result = match word {
        "fire" => Some(AttackType::Fire),
        "radiation" => Some(AttackType::Radiation),
        "cold" => Some(AttackType::Cold),
        "slashing" => Some(AttackType::Slash),
        "bludgeoning" => Some(AttackType::Bludgeon),
        _ => {
          println!("Unknown attack: {}", word);
          None
        }
      };

      result.unwrap()
    }

    fn parse_stats(stats: &str, regs: &Regs) -> (Vec<AttackType>, Vec<AttackType>) {
      let mut immune_to = vec![];
      let mut weak_to = vec![];

      if stats != "" {
        let (_, reg_immune, reg_weak, reg_word) = regs;

        for cap_immune in reg_immune.captures_iter(stats) {
          for cap_immune_word in reg_word.captures_iter(&cap_immune[1]) {
            immune_to.push(parse_attack_type(&cap_immune_word[1]));
          }
        }

        for cap_weak in reg_weak.captures_iter(stats) {
          for cap_weak_word in reg_word.captures_iter(&cap_weak[1]) {
            weak_to.push(parse_attack_type(&cap_weak_word[1]));
          }
        }
      }

      (immune_to, weak_to)
    }

    let (line_reg, _, _, _) = regs;
    let caps = line_reg.captures(line).unwrap();
    let (immune_to, weak_to) = parse_stats(caps.get(3).unwrap().as_str(), regs);

    let mut group = Group {
      id,
      units_num: caps.get(1).unwrap().as_str().parse::<usize>().unwrap(),
      hit_points: caps.get(2).unwrap().as_str().parse::<usize>().unwrap(),
      group_type,
      immune_to,
      weak_to,
      attack_damage: caps.get(4).unwrap().as_str().parse::<usize>().unwrap(),
      attack_type: parse_attack_type(caps.get(5).unwrap().as_str()),
      initiative: caps.get(6).unwrap().as_str().parse::<usize>().unwrap(),
    };

    group
  }

  fn run_battle(groups: &mut Vec<Group>) {
    fn get_fight_selections(groups: &mut Vec<Group>) -> HashMap<GroupId, Option<GroupId>> {
      let mut fight_selections = HashMap::new();
      let mut attacked_groups: HashSet<GroupId> = HashSet::new();
      let mut attacking_groups = groups.clone();

      attacking_groups.sort_by(|a, b| {
        match b.get_effective_power().cmp(&a.get_effective_power()) {
          Ordering::Equal => b.initiative.cmp(&a.initiative),
          v => v,
        }
      });

      for attacking_group in attacking_groups {
        let mut max_damage = 0;
        let mut attacked_candidates: Vec<Group> = vec![];

        for defending_group in groups.clone() {
          if defending_group.id == attacking_group.id
            || defending_group.group_type == attacking_group.group_type
            || attacked_groups.contains(&defending_group.id)
          {
            continue;
          }

          let damage = attacking_group.get_damage_to_group(&defending_group);

          match damage.cmp(&max_damage) {
            Ordering::Equal => {
              attacked_candidates.push(defending_group.clone());
            }
            Ordering::Greater => {
              max_damage = damage;
              attacked_candidates = vec![];
              attacked_candidates.push(defending_group.clone());
            }
            _ => {}
          }
        }
      }

      std::process::exit(1);

      fight_selections
    }

    loop {
      let fight_selections = get_fight_selections(groups);
      // attacks phase
    }
  }

  fn get_effective_power(&self) -> usize {
    self.units_num * self.attack_damage
  }

  fn get_damage_to_group(&self, defending_group: &Group) -> usize {
    if defending_group.immune_to.contains(&self.attack_type) {
      return 0;
    }

    let effective_power = self.get_effective_power();

    if defending_group.weak_to.contains(&self.attack_type) {
      return effective_power * 2;
    }

    effective_power
  }
}

fn get_input_groups() -> Vec<Group> {
  let mut file = File::open("src/input.txt").expect("Unable to open the file");
  let mut contents = String::new();
  file
    .read_to_string(&mut contents)
    .expect("Unable to read the file");

  Group::new_from_text(&contents)
}

fn main() {
  let mut groups = get_input_groups();

  Group::run_battle(&mut groups);

  println!("Results:");
}

#[cfg(test)]
mod tests {
  use super::*;

  fn get_example_data() -> String {
    "Immune System:
17 units each with 5390 hit points (weak to radiation, bludgeoning) with an attack that does 4507 fire damage at initiative 2
989 units each with 1274 hit points (immune to fire; weak to bludgeoning, slashing) with an attack that does 25 slashing damage at initiative 3

Infection:
801 units each with 4706 hit points (weak to radiation) with an attack that does 116 bludgeoning damage at initiative 1
4485 units each with 2961 hit points (immune to radiation; weak to fire, cold) with an attack that does 12 slashing damage at initiative 4".to_string()
  }

  #[test]
  fn test_new_from_line() {
    let line_1 = "8233 units each with 2012 hit points (immune to radiation) with an attack that does 2 fire damage at initiative 5";
    let line_2 = "115 units each with 10354 hit points (immune to fire, radiation, bludgeoning) with an attack that does 788 cold damage at initiative 2";

    assert_eq!(
      Group::new_from_line(&line_1, GroupType::Immune, &Group::get_regs(), 0),
      Group {
        id: 0,
        units_num: 8233,
        hit_points: 2012,
        immune_to: vec![AttackType::Radiation],
        weak_to: vec![],
        group_type: GroupType::Immune,
        attack_damage: 2,
        attack_type: AttackType::Fire,
        initiative: 5,
      }
    );
    assert_eq!(
      Group::new_from_line(&line_2, GroupType::Immune, &Group::get_regs(), 0),
      Group {
        id: 0,
        units_num: 115,
        hit_points: 10354,
        immune_to: vec![
          AttackType::Fire,
          AttackType::Radiation,
          AttackType::Bludgeon
        ],
        weak_to: vec![],
        attack_damage: 788,
        group_type: GroupType::Immune,
        attack_type: AttackType::Cold,
        initiative: 2,
      }
    );
  }

  #[test]
  fn test_run_battle() {
    let example_data = get_example_data();
    let mut groups = Group::new_from_text(&example_data);

    Group::run_battle(&mut groups);
  }
}
