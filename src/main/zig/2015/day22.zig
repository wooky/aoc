const aoc = @import("../aoc.zig");
const std = @import("std");

const EffectTimer = u8;
const HitPoints = u8;
const Mana = u16;

const TurnResult = enum {
    CONTINUE, WIN, FAIL,
};

const InstantSpell = struct { cost: Mana, heal: HitPoints, damage: HitPoints };
const Spell = union(enum) {
    Instant: InstantSpell,
    Shield: void,
    Poison: void,
    Recharge: void,
};

const Player = struct {
    const shield_effect: HitPoints = 7;
    const recharge_effect: Mana = 101;

    const shield_cost: Mana = 113;
    const poison_cost: Mana = 173;
    const rechage_cost: Mana = 229;

    const shield_time: EffectTimer = 6;
    const poison_time: EffectTimer = 6;
    const recharge_time: EffectTimer = 5;

    hp: HitPoints = 50,
    mana_remaining: Mana = 500,
    mana_used: Mana = 0,
    shield_effect_timer: EffectTimer = 0,
    recharge_effect_timer: EffectTimer = 0,
    armor: HitPoints = undefined,

    fn damage(self: *Player, dmg: HitPoints) bool {
        if (dmg >= self.hp) {
            return true;
        }
        self.hp -= dmg;
        return false;
    }

    fn tick(self: *Player) void {
        if (self.shield_effect_timer != 0) {
            self.armor = shield_effect;
            self.shield_effect_timer -= 1;
        }
        else {
            self.armor = 0;
        }
        
        if (self.recharge_effect_timer != 0) {
            self.mana_remaining += recharge_effect;
            self.recharge_effect_timer -= 1;
        }
    }

    fn castSpell(self: *Player, spell: Spell, boss: *Boss) TurnResult {
        return switch (spell) {
            .Instant => |s| self.applyInstantSpell(s, boss),
            .Shield => self.applyShieldEffect(),
            .Poison => self.applyPoisonEffect(boss),
            .Recharge => self.applyRechargeEffect(),
        };
    }

    fn applyInstantSpell(self: *Player, spell: InstantSpell, boss: *Boss) TurnResult {
        if (!self.consumeMana(spell.cost)) {
            return .FAIL;
        }
        if (boss.damage(spell.damage)) {
            return .WIN;
        }
        self.hp += spell.heal;
        return .CONTINUE;
    }

    fn applyShieldEffect(self: *Player) TurnResult {
        if (self.shield_effect_timer != 0 or !self.consumeMana(shield_cost)) {
            return .FAIL;
        }
        self.shield_effect_timer = shield_time;
        return .CONTINUE;
    }

    fn applyPoisonEffect(self: *Player, boss: *Boss) TurnResult {
        if (boss.poison_effect_timer != 0 or !self.consumeMana(poison_cost)) {
            return .FAIL;
        }
        boss.poison_effect_timer = poison_time;
        return .CONTINUE;
    }

    fn applyRechargeEffect(self: *Player) TurnResult {
        if (self.recharge_effect_timer != 0 or !self.consumeMana(rechage_cost)) {
            return .FAIL;
        }
        self.recharge_effect_timer = recharge_time;
        return .CONTINUE;
    }

    fn consumeMana(self: *Player, mana: Mana) bool {
        if (mana > self.mana_remaining) {
            return false;
        }
        self.mana_remaining -= mana;
        self.mana_used += mana;
        return true;
    }
};

const Boss = struct {
    const damage_effect: HitPoints = 3;

    hp: HitPoints,
    atk: HitPoints,
    poison_effect_timer: EffectTimer = 0,

    fn damage(self: *Boss, dmg: HitPoints) bool {
        if (dmg >= self.hp) {
            return true;
        }
        self.hp -= dmg;
        return false;
    }

    fn tick(self: *Boss) bool {
        if (self.poison_effect_timer != 0) {
            if (self.damage(damage_effect)) {
                return true;
            }
            self.poison_effect_timer -= 1;
        }
        return false;
    }

    fn attack(self: *Boss, player: *Player) bool {
        return player.damage(self.atk - player.armor);
    }
};

const spells = [_]Spell {
    .{ .Instant = .{ .cost = 53, .heal = 0, .damage = 4 } },
    .{ .Instant = .{ .cost = 73, .heal = 2, .damage = 2 } },
    .{ .Shield = {} },
    .{ .Poison = {} },
    .{ .Recharge = {} },
};

pub fn run(problem: *aoc.Problem) !aoc.Solution {
    var tokens = std.mem.tokenize(u8, problem.input, ": \n");
    _ = tokens.next().?; _ = tokens.next().?;
    const boss_hp = try std.fmt.parseInt(HitPoints, tokens.next().?, 10);
    _ = tokens.next().?;
    const boss_atk = try std.fmt.parseInt(HitPoints, tokens.next().?, 10);

    const res1 = blk: {
        var player = Player {};
        var boss = Boss { .hp = boss_hp, .atk = boss_atk };
        break :blk getMinManaUsed(&player, &boss, std.math.maxInt(Mana), 0);
    };

    const res2 = blk: {
        var player = Player {};
        var boss = Boss { .hp = boss_hp, .atk = boss_atk };
        break :blk getMinManaUsed(&player, &boss, std.math.maxInt(Mana), 1);
    };
    
    return problem.solution(res1, res2);
}

fn getMinManaUsed(player: *Player, boss: *Boss, max_mana: Mana, player_damage_per_turn: HitPoints) Mana {
    var min_mana = max_mana;

    if (player.damage(player_damage_per_turn)) {
        return min_mana;
    }
    player.tick();
    if (boss.tick()) {
        return player.mana_used;
    }

    for (spells) |spell| {
        var new_player = player.*;
        var new_boss = boss.*;
        const result = new_player.castSpell(spell, &new_boss);
        if (new_player.mana_used >= min_mana or result == .FAIL) {
            continue;
        }
        if (result == .WIN) {
            min_mana = new_player.mana_used;
            continue;
        }
        new_player.tick();
        if (new_boss.tick()) {
            min_mana = new_player.mana_used;
            continue;
        }
        if (new_boss.attack(&new_player)) {
            continue;
        }
        min_mana = getMinManaUsed(&new_player, &new_boss, min_mana, player_damage_per_turn);
    }

    return min_mana;
}
