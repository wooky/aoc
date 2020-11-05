const aoc = @import("../aoc.zig");
const std = @import("std");
const Mana = u16;

const shield_effect: i8 = 7;
const damage_effect: u8 = 3;
const mana_effect: u8 = 101;

const Spell = struct {
    cost: Mana, instant_damage: u8 = 0, instant_heal: u8 = 0,
    shield_timer: u8 = 0, damage_timer: u8 = 0, mana_timer: u8 = 0
};

const State = struct {
    boss_hp: u8, boss_atk: i8, player_hp: u8 = 50, mana_used: Mana = 0, mana_remaining: Mana = 500,
    shield_timer: u8 = 0, damage_timer: u8 = 0, mana_timer: u8 = 0
};

const spells = [_]Spell {
    .{ .cost = 53, .instant_damage = 4 },
    .{ .cost = 73, .instant_damage = 2, .instant_heal = 2 },
    .{ .cost = 113, .shield_timer = 6 },
    .{ .cost = 173, .damage_timer = 6 },
    .{ .cost = 229, .mana_timer = 5 },
};

pub fn run(problem: *aoc.Problem) !void {
    var tokens = std.mem.tokenize(problem.input, ": \n");
    _ = tokens.next().?; _ = tokens.next().?;
    const boss_hp = try std.fmt.parseInt(u8, tokens.next().?, 10);
    _ = tokens.next().?;
    const boss_atk = try std.fmt.parseInt(i8, tokens.next().?, 10);

    const min_mana_used = performSpell(
        State { .boss_hp = boss_hp, .boss_atk = boss_atk },
        Spell { .cost = 0, .instant_heal = @intCast(u8, boss_atk) }, // Disgusting hack
        std.math.maxInt(Mana), 0
    );
    std.debug.warn("{}\n", .{min_mana_used});
}

fn performSpell(state: State, spell: Spell, min_mana_used: Mana, spaces: u8) Mana {
    // var i: u8 = 0; while (i < spaces) : (i += 1) std.debug.warn(" ", .{});
    // std.debug.warn("{} {} {}\n", .{spell, state, min_mana_used});
    var new_state = state;

    // Apply effects, return on invalid state or if enemy dies from effect
    var def: i8 = 0;
    if (state.shield_timer > 0) {
        if (spell.shield_timer > 0) {
            // std.debug.warn("Can't reactivate shield\n", .{});
            return min_mana_used;
        }
        new_state.shield_timer -= 1;
        def = shield_effect;
    }
    if (state.damage_timer > 0) {
        if (spell.damage_timer > 0) {
            // std.debug.warn("Can't reactivate damage\n", .{});
            return min_mana_used;
        }
        if (damage_effect >= state.boss_hp) {
            return state.mana_used;
        }
        new_state.damage_timer -= 1;
        new_state.boss_hp -= damage_effect;
    }
    if (state.mana_timer > 0) {
        if (spell.mana_timer > 0) {
            // std.debug.warn("Can't reactivate mana\n", .{});
            return min_mana_used;
        }
        new_state.mana_timer -= 1;
        new_state.mana_remaining += mana_effect;
    }

    // Cast spell, return if it's too expensive
    new_state.mana_used += spell.cost;
    if (new_state.mana_used >= min_mana_used or new_state.mana_remaining < spell.cost) {
        // std.debug.warn("Spell too expensive\n", .{});
        return min_mana_used;
    }
    new_state.mana_remaining -= spell.cost;

    // Apply damage and health, return if someone dies
    if (spell.instant_damage >= new_state.boss_hp) {
        return new_state.mana_used;
    }
    new_state.boss_hp -= spell.instant_damage;
    new_state.player_hp += spell.instant_heal;
    const boss_atk = @intCast(u8, std.math.max(state.boss_atk - def, 1));
    if (boss_atk >= new_state.player_hp) {
        // std.debug.warn("RIP\n", .{});
        return min_mana_used;
    }
    new_state.player_hp -= boss_atk;

    // Apply timers
    new_state.shield_timer = std.math.max(new_state.shield_timer, spell.shield_timer);
    new_state.damage_timer = std.math.max(new_state.damage_timer, spell.damage_timer);
    new_state.mana_timer = std.math.max(new_state.mana_timer, spell.mana_timer);

    // Iterate through next spells to get new min mana used
    var new_min_mana_used = min_mana_used;
    for (spells) |next_spell| {
        new_min_mana_used = performSpell(new_state, next_spell, new_min_mana_used, spaces + 1);
    }
    return new_min_mana_used;
}
