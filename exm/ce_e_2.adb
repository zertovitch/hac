--  Run-time Constraint_Error following a subtype Range Check

procedure CE_e_2 is

  type Compression_Method is
   (Store,
    Shrink,
    Reduce_1,
    Reduce_2,
    Reduce_3,
    Reduce_4,
    --
    Deflate_Fixed,
    Deflate_0,
    Deflate_1,
    Deflate_2,
    Deflate_3,
    Deflate_R,
    --
    LZMA_0,
    LZMA_1,
    LZMA_2,
    LZMA_3);

  subtype Reduction_Method is Compression_Method range Reduce_1 .. Reduce_4;
  subtype Deflation_Method is Compression_Method range Deflate_Fixed .. Deflate_R;

  i : Deflation_Method;
  j : Compression_Method := Reduce_3;
  k : Reduction_Method;

begin
  i := j;
end CE_e_2;