const c = @cImport({
    @cInclude("gsl/gsl_matrix.h");
    @cInclude("gsl/gsl_blas.h");
});
const std = @import("std");
const aoc = @import("../aoc.zig");

pub const SquareMatrix = struct {
    var flip_matrix: ?SquareMatrix = null;

    matrix: *c.gsl_matrix,

    pub fn init(side: usize) SquareMatrix {
        return .{
            .matrix = c.gsl_matrix_calloc(side, side),
        };
    }

    pub fn deinit(self: *SquareMatrix) void {
        self.freeMatrix();
    }

    inline fn freeMatrix(self: *SquareMatrix) void {
        c.gsl_matrix_free(self.matrix);
    }

    pub inline fn size(self: *const SquareMatrix) usize {
        return self.matrix.*.size1;
    }

    pub fn get(self: *const SquareMatrix, coord: aoc.Coord) f64 {
        return c.gsl_matrix_get(self.matrix, @intCast(usize, coord.row), @intCast(usize, coord.col));
    }

    pub fn set(self: *SquareMatrix, coord: aoc.Coord, x: f64) void {
        c.gsl_matrix_set(self.matrix, @intCast(usize, coord.row), @intCast(usize, coord.col), x);
    }

    pub fn submatrix(self: *const SquareMatrix, begin: aoc.Coord, dimensions: aoc.Coord) MatrixView {
        return .{ .view = c.gsl_matrix_const_submatrix(
            self.matrix,
            @intCast(usize, begin.row),
            @intCast(usize, begin.col),
            @intCast(usize, dimensions.row),
            @intCast(usize, dimensions.col)
        ) };
    }

    pub fn rotate90DegreesClockwise(self: *SquareMatrix) void {
        var result = SquareMatrix.init(self.size());
        _ = c.gsl_blas_dgemm(.CblasTrans, .CblasNoTrans, 1, self.matrix, self.getFlipMatrix().matrix, 0, result.matrix);
        self.freeMatrix();
        self.matrix = result.matrix;
    }

    pub fn flipHorizontally(self: *SquareMatrix) void {
        var result = SquareMatrix.init(self.size());
        _ = c.gsl_blas_dgemm(.CblasNoTrans, .CblasNoTrans, 1, self.matrix, self.getFlipMatrix().matrix, 0, result.matrix);
        self.freeMatrix();
        self.matrix = result.matrix;
    }

    fn getFlipMatrix(self: *SquareMatrix) SquareMatrix {
        if (flip_matrix != null and flip_matrix.?.size() != self.size()) {
            flip_matrix.?.deinit();
            flip_matrix = null;
        }
        if (flip_matrix == null) {
            flip_matrix = SquareMatrix.init(self.size());
            var i: isize = 0;
            while (i < self.size()) : (i += 1) {
                flip_matrix.?.set(aoc.Coord.fromRowCol(i, @intCast(isize, self.size()) - i - 1), 1);
            }
        }
        return flip_matrix.?;
    }

    pub fn printMatrix(self: *const SquareMatrix) void {
        var row: usize = 0;
        while (row < self.size()) : (row += 1) {
            var col: usize = 0;
            while (col < self.size()) : (col += 1) {
                std.debug.print("{d},", .{self.get(row, col)});
            }
            std.debug.print("\n", .{});
        }
    }
};

pub const MatrixView = struct {
    view: c.gsl_matrix_const_view,

    pub fn equals(self: *const MatrixView, other: *const MatrixView) bool {
        return c.gsl_matrix_equal(&self.view.matrix, &other.view.matrix) == 1;
    }
};
