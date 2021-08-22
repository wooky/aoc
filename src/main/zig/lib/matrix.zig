const c = @cImport({
    @cInclude("gsl/gsl_matrix.h");
    @cInclude("gsl/gsl_blas.h");
});
const std = @import("std");

pub const SquareMatrix = struct {
    var flip_matrix: ?SquareMatrix = null;

    side: usize,
    matrix: *c.gsl_matrix,

    pub fn init(side: usize) SquareMatrix {
        return .{
            .side = side,
            .matrix = c.gsl_matrix_calloc(side, side),
        };
    }

    pub fn deinit(self: *SquareMatrix) void {
        self.freeMatrix();
    }

    inline fn freeMatrix(self: *SquareMatrix) void {
        c.gsl_matrix_free(self.matrix);
    }

    pub fn get(self: *const SquareMatrix, row: usize, col: usize) f64 {
        return c.gsl_matrix_get(self.matrix, row, col);
    }

    pub fn set(self: *SquareMatrix, row: usize, col: usize, x: f64) void {
        c.gsl_matrix_set(self.matrix, row, col, x);
    }

    pub fn submatrix(self: *const SquareMatrix, y: usize, x: usize, rows: usize, cols: usize) MatrixView {
        return .{ .view = c.gsl_matrix_const_submatrix(self.matrix, y, x, rows, cols) };
    }

    pub fn rotate90DegreesClockwise(self: *SquareMatrix) void {
        var result = SquareMatrix.init(self.side);
        _ = c.gsl_blas_dgemm(.CblasTrans, .CblasNoTrans, 1, self.matrix, self.getFlipMatrix().matrix, 0, result.matrix);
        self.freeMatrix();
        self.matrix = result.matrix;
    }

    pub fn flipHorizontally(self: *SquareMatrix) void {
        var result = SquareMatrix.init(self.side);
        _ = c.gsl_blas_dgemm(.CblasNoTrans, .CblasNoTrans, 1, self.matrix, self.getFlipMatrix().matrix, 0, result.matrix);
        self.freeMatrix();
        self.matrix = result.matrix;
    }

    fn getFlipMatrix(self: *SquareMatrix) SquareMatrix {
        if (flip_matrix != null and flip_matrix.?.side != self.side) {
            flip_matrix.?.deinit();
            flip_matrix = null;
        }
        if (flip_matrix == null) {
            flip_matrix = SquareMatrix.init(self.side);
            var i: usize = 0;
            while (i < self.side) : (i += 1) {
                flip_matrix.?.set(i, self.side - i - 1, 1);
            }
        }
        return flip_matrix.?;
    }

    pub fn printMatrix(self: *const SquareMatrix) void {
        var row: usize = 0;
        while (row < self.side) : (row += 1) {
            var col: usize = 0;
            while (col < self.side) : (col += 1) {
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
