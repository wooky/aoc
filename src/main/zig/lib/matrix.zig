const c = @cImport({
    @cInclude("gsl/gsl_matrix.h");
    @cInclude("gsl/gsl_blas.h");
});
const std = @import("std");

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
            var i: usize = 0;
            while (i < self.size()) : (i += 1) {
                flip_matrix.?.set(i, self.size() - i - 1, 1);
            }
        }
        return flip_matrix.?;
    }

    pub fn copyFrom(self: *SquareMatrix, src: *const SquareMatrix, src_y: usize, src_x: usize, dst_y: usize, dst_x: usize, rows: usize, cols: usize) void {
        var row: usize = 0;
        while (row < rows) : (row += 1) {
            var col: usize = 0;
            while (col < cols) : (col += 1) {
                self.set(dst_y + row, dst_x + col, src.get(src_y + row, src_x + col));
            }
        }
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
