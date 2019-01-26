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

--- Part Two ---

Things aren't looking good for the reindeer. The man asks whether more milk and cookies would help
you think.

If only you could give the reindeer's immune system a boost, you might be able to change the
outcome of the combat.

A boost is an integer increase in immune system units' attack damage. For example, if you were to
boost the above example's immune system's units by 1570, the armies would instead look like this:

Immune System:
17 units each with 5390 hit points (weak to radiation, bludgeoning) with
 an attack that does 6077 fire damage at initiative 2
989 units each with 1274 hit points (immune to fire; weak to bludgeoning,
 slashing) with an attack that does 1595 slashing damage at initiative 3

Infection:
801 units each with 4706 hit points (weak to radiation) with an attack
 that does 116 bludgeoning damage at initiative 1
4485 units each with 2961 hit points (immune to radiation; weak to fire,
 cold) with an attack that does 12 slashing damage at initiative 4
With this boost, the combat proceeds differently:

Immune System:
Group 2 contains 989 units
Group 1 contains 17 units
Infection:
Group 1 contains 801 units
Group 2 contains 4485 units

Infection group 1 would deal defending group 2 185832 damage
Infection group 1 would deal defending group 1 185832 damage
Infection group 2 would deal defending group 1 53820 damage
Immune System group 2 would deal defending group 1 1577455 damage
Immune System group 2 would deal defending group 2 1577455 damage
Immune System group 1 would deal defending group 2 206618 damage

Infection group 2 attacks defending group 1, killing 9 units
Immune System group 2 attacks defending group 1, killing 335 units
Immune System group 1 attacks defending group 2, killing 32 units
Infection group 1 attacks defending group 2, killing 84 units
Immune System:
Group 2 contains 905 units
Group 1 contains 8 units
Infection:
Group 1 contains 466 units
Group 2 contains 4453 units

Infection group 1 would deal defending group 2 108112 damage
Infection group 1 would deal defending group 1 108112 damage
Infection group 2 would deal defending group 1 53436 damage
Immune System group 2 would deal defending group 1 1443475 damage
Immune System group 2 would deal defending group 2 1443475 damage
Immune System group 1 would deal defending group 2 97232 damage

Infection group 2 attacks defending group 1, killing 8 units
Immune System group 2 attacks defending group 1, killing 306 units
Infection group 1 attacks defending group 2, killing 29 units
Immune System:
Group 2 contains 876 units
Infection:
Group 2 contains 4453 units
Group 1 contains 160 units

Infection group 2 would deal defending group 2 106872 damage
Immune System group 2 would deal defending group 2 1397220 damage
Immune System group 2 would deal defending group 1 1397220 damage

Infection group 2 attacks defending group 2, killing 83 units
Immune System group 2 attacks defending group 2, killing 427 units
After a few fights...

Immune System:
Group 2 contains 64 units
Infection:
Group 2 contains 214 units
Group 1 contains 19 units

Infection group 2 would deal defending group 2 5136 damage
Immune System group 2 would deal defending group 2 102080 damage
Immune System group 2 would deal defending group 1 102080 damage

Infection group 2 attacks defending group 2, killing 4 units
Immune System group 2 attacks defending group 2, killing 32 units
Immune System:
Group 2 contains 60 units
Infection:
Group 1 contains 19 units
Group 2 contains 182 units

Infection group 1 would deal defending group 2 4408 damage
Immune System group 2 would deal defending group 1 95700 damage
Immune System group 2 would deal defending group 2 95700 damage

Immune System group 2 attacks defending group 1, killing 19 units
Immune System:
Group 2 contains 60 units
Infection:
Group 2 contains 182 units

Infection group 2 would deal defending group 2 4368 damage
Immune System group 2 would deal defending group 2 95700 damage

Infection group 2 attacks defending group 2, killing 3 units
Immune System group 2 attacks defending group 2, killing 30 units
After a few more fights...

Immune System:
Group 2 contains 51 units
Infection:
Group 2 contains 40 units

Infection group 2 would deal defending group 2 960 damage
Immune System group 2 would deal defending group 2 81345 damage

Infection group 2 attacks defending group 2, killing 0 units
Immune System group 2 attacks defending group 2, killing 27 units
Immune System:
Group 2 contains 51 units
Infection:
Group 2 contains 13 units

Infection group 2 would deal defending group 2 312 damage
Immune System group 2 would deal defending group 2 81345 damage

Infection group 2 attacks defending group 2, killing 0 units
Immune System group 2 attacks defending group 2, killing 13 units
Immune System:
Group 2 contains 51 units
Infection:
No groups remain.

This boost would allow the immune system's armies to win! It would be left with 51 units.

You don't even know how you could boost the reindeer's immune system or what
effect it might have, so you need to be cautious and find the smallest boost
that would allow the immune system to win.

How many units does the immune system have left after getting the smallest boost it needs to win?

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

    for (_, line) in text.lines().enumerate() {
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

    Group {
      id,
      units_num: caps.get(1).unwrap().as_str().parse::<usize>().unwrap(),
      hit_points: caps.get(2).unwrap().as_str().parse::<usize>().unwrap(),
      group_type,
      immune_to,
      weak_to,
      attack_damage: caps.get(4).unwrap().as_str().parse::<usize>().unwrap(),
      attack_type: parse_attack_type(caps.get(5).unwrap().as_str()),
      initiative: caps.get(6).unwrap().as_str().parse::<usize>().unwrap(),
    }
  }

  fn run_battle(mut groups: &mut Vec<Group>) {
    fn get_fight_selections(groups: &mut Vec<Group>) -> HashMap<GroupId, Option<GroupId>> {
      let mut fight_selections = HashMap::new();
      let mut attacked_groups: HashSet<GroupId> = HashSet::new();

      groups.sort_by(
        |a, b| match b.get_effective_power().cmp(&a.get_effective_power()) {
          Ordering::Equal => b.initiative.cmp(&a.initiative),
          v => v,
        },
      );

      let attacking_groups = groups.clone();

      for attacking_group in attacking_groups {
        let mut max_damage = 0;
        let mut attacked_candidates: Vec<Group> = vec![];

        for defending_group in groups.clone() {
          if defending_group.group_type == attacking_group.group_type
            || attacked_groups.contains(&defending_group.id)
            || defending_group.units_num == 0
          {
            continue;
          }

          let damage = attacking_group.get_damage_to_group(&defending_group);

          if damage == 0 {
            continue;
          }

          match damage.cmp(&max_damage) {
            Ordering::Equal => {
              attacked_candidates.push(defending_group.clone());
            }
            Ordering::Greater => {
              max_damage = damage;
              attacked_candidates = vec![defending_group.clone()];
            }
            _ => {}
          }
        }

        let attacked_group_id = if !attacked_candidates.is_empty() {
          attacked_groups.insert(attacked_candidates[0].id);
          Some(attacked_candidates[0].id)
        } else {
          None
        };

        fight_selections.insert(attacking_group.id, attacked_group_id);
      }

      fight_selections
    }

    fn perform_attacks(
      groups: &mut Vec<Group>,
      fight_selections: &HashMap<usize, Option<usize>>,
    ) -> bool {
      groups.sort_by(|a, b| b.initiative.cmp(&a.initiative));
      let mut were_attacks = false;

      let mut attacks_num = 0;
      let mut last_attacked_group_id = 0;
      let mut last_attacking_group_id = 0;

      for (key, id) in fight_selections {
        if fight_selections[&key].is_some() {
          attacks_num += 1;
          last_attacked_group_id = id.unwrap();
          last_attacking_group_id = *key;
        }
      }

      // optimization
      if attacks_num == 1 {
        let attacked_group_index = groups
          .iter()
          .position(|r| r.id == last_attacked_group_id)
          .unwrap();
        let attacking_group_idx = groups
          .iter()
          .position(|r| r.id == last_attacking_group_id)
          .unwrap();

        let damage = groups[attacking_group_idx].get_damage_to_group(&groups[attacked_group_index]);

        if damage > 0 && damage > groups[attacked_group_index].hit_points {
          groups[attacked_group_index].units_num = 0;

          return true;
        } else {
          return false;
        }
      }

      for attacking_group in groups.clone() {
        let attacked_group_id = fight_selections[&attacking_group.id];
        let mut attacking_group_idx = groups
          .iter()
          .position(|r| r.id == attacking_group.id)
          .unwrap();

        if groups[attacking_group_idx].units_num > 0 && attacked_group_id.is_some() {
          let attacked_group_index = groups
            .iter()
            .position(|r| r.id == attacked_group_id.unwrap())
            .unwrap();

          let damage =
            groups[attacking_group_idx].get_damage_to_group(&groups[attacked_group_index]);

          groups[attacked_group_index].receive_damage(damage);

          if damage > 0 {
            were_attacks = true;
          }
        }
      }

      were_attacks
    }

    loop {
      let fight_selections = get_fight_selections(groups);

      let were_attacks = perform_attacks(&mut groups, &fight_selections);

      let mut remaining_groups: HashSet<GroupType> = HashSet::new();

      for group in groups.clone() {
        if group.units_num > 0 {
          remaining_groups.insert(group.group_type);
        }
      }

      if remaining_groups.len() < 2 || !were_attacks {
        break;
      }
    }
  }

  fn get_units_on_min_immune_win(groups: &[Group]) -> usize {
    let mut boost = 1;

    loop {
      let mut used_groups = groups.to_owned();

      for mut group in used_groups.iter_mut() {
        if group.group_type == GroupType::Immune {
          group.attack_damage += boost;
        }
      }

      Group::run_battle(&mut used_groups);

      let winning_group = Group::get_winning_group(&used_groups);

      if winning_group == Some(GroupType::Immune) {
        return Group::get_remaining_units_for_group_type(&used_groups, GroupType::Immune);
      }

      boost += 1;
    }
  }

  fn get_remaining_units_for_groups(groups: &[Group]) -> usize {
    groups.iter().fold(0, |sum, x| x.units_num + sum)
  }

  fn get_remaining_units_for_group_type(groups: &[Group], group_type: GroupType) -> usize {
    groups.iter().fold(0, |sum, x| {
      if x.group_type == group_type {
        sum + x.units_num
      } else {
        sum
      }
    })
  }

  fn get_winning_group(groups: &[Group]) -> Option<GroupType> {
    let mut winners: HashSet<GroupType> = HashSet::new();

    for group in groups {
      if group.units_num > 0 {
        winners.insert(group.group_type);
      }
    }

    if winners.len() == 1 {
      return Some(*winners.iter().nth(0).unwrap());
    }

    None
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

  fn receive_damage(&mut self, damage: usize) {
    let units_lost = damage / self.hit_points;
    let new_units = std::cmp::max(self.units_num as i32 - units_lost as i32, 0);

    self.units_num = new_units as usize;
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
  let cloned_groups = groups.clone();

  Group::run_battle(&mut groups);

  let remaining_units = Group::get_remaining_units_for_groups(&groups);

  let remaining_units_2 = Group::get_units_on_min_immune_win(&cloned_groups);

  println!("Results:");
  println!("- (1) remaining units {}", remaining_units);
  println!("- (2) remaining units {}", remaining_units_2);
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

    let remaining_units = Group::get_remaining_units_for_groups(&groups);

    assert_eq!(remaining_units, 5216);
  }

  #[test]
  fn test_get_units_on_min_immune_win() {
    let example_data = get_example_data();
    let groups = Group::new_from_text(&example_data);
    let units_num = Group::get_units_on_min_immune_win(&groups);

    assert_eq!(units_num, 51);
  }
}
