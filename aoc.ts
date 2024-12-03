import { run as gleamRun } from "./build/glm-obj/dev/javascript/aoc_glm/aoc_glm.mjs";
import runners from "./runners.json" with { type: "json" };
import process from "node:process";

type Solution = [string, string];
const extractString = (dv: DataView, offset: number): string => {
  const ptr = Deno.UnsafePointer.create(dv.getBigUint64(offset, true));
  if (ptr === null) throw new Error(`Pointer at offset ${offset} is null`);
  return new Deno.UnsafePointerView(ptr).getCString();
};
const extractSolution = (result: Uint8Array): Solution => {
  const dv = new DataView(result.buffer, result.byteOffset, result.byteLength);
  return [extractString(dv, 0), extractString(dv, 8)];
};

const runnerMapping = {
  "ada": (input: Uint8Array, year: number, day: number): Solution => {
    const {
      symbols: {
        run,
        aoc_adainit,
        aoc_adafinal,
      },
    } = Deno.dlopen(
      "build/libaoc_ada.so",
      {
        run: {
          parameters: ["buffer", "u16", "u16"],
          result: { struct: ["pointer", "pointer"] },
        },
        aoc_adainit: {
          parameters: [],
          result: "void",
        },
        aoc_adafinal: {
          parameters: [],
          result: "void",
        },
      },
    );

    aoc_adainit();
    const solution = extractSolution(run(input, year, day));
    aoc_adafinal();
    return solution;
  },

  "cxx": (input: Uint8Array, year: number, day: number): Solution =>
    extractSolution(
      Deno.dlopen(
        "build/libaoc_cxx.so",
        {
          run: {
            parameters: ["buffer", "u16", "u16"],
            result: { struct: ["pointer", "pointer"] },
          },
        },
      ).symbols.run(input, year, day),
    ),

  "glm": (input: Uint8Array, year: number, day: number): Solution =>
    gleamRun(input, year, day) as [string, string],

  "nim": (input: Uint8Array, year: number, day: number): Solution =>
    extractSolution(
      Deno.dlopen(
        "build/libaoc_nim.so",
        {
          run: {
            parameters: ["buffer", "u16", "u16"],
            result: { struct: ["pointer", "pointer"] },
          },
        },
      ).symbols.run(input, year, day),
    ),

  "zig": (input: Uint8Array, year: number, day: number): Solution =>
    extractSolution(
      Deno.dlopen(
        "build/libaoc_zig.so",
        {
          run: {
            parameters: ["buffer", "u16", "u16"],
            result: { struct: ["pointer", "pointer"] },
          },
        },
      ).symbols.run(input, year, day),
    ),
};

const year = parseInt(process.argv[2]);
const day = parseInt(process.argv[3]);
if (isNaN(year) || isNaN(day)) {
  throw new Error(`Usage: ${process.argv[0]} ${process.argv[1]} [year] [day]`);
}

let runnerType: string | object = runners[year.toString()];
if (typeof runnerType === "object") {
  runnerType = runnerType[day.toString()];
}
if (typeof runnerType !== "string") {
  throw new Error("Invalid year/day");
}
const runner = runnerMapping[runnerType];

const text = await Deno.readTextFile(
  `input/${year}/day${day.toString().padStart(2, "0")}.txt`,
);
const input = new TextEncoder().encode(`${text}\0`);
const [s1, s2] = runner(input, year, day);
console.log(s1);
console.log(s2);
