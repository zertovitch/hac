--  Compile-time Constraint_Error following a subtype Range Check

procedure CE_e_1 is

  type Compression_Method is
   (Store,
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

  subtype Deflation_Method is Compression_Method range Deflate_Fixed .. Deflate_R;

  subtype Correct is Deflation_Method range Deflate_1 .. Deflate_3;
  subtype Wrong   is Deflation_Method range LZMA_0 .. LZMA_3;

begin
  null;
end CE_e_2;