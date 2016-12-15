module Output (OutputFormat(..), outputObj) where

import Insn
import Mnemonic (assemble)
import Program
 
import qualified Data.ByteString.Lazy as BL
import Data.Binary.Put
import Data.Bits (complement)
import Data.Int (Int64)
import Data.Word (Word, Word8, Word32)
import Text.Printf (printf)

data OutputFormat = Progrom | IntelHex deriving (Show, Read, Enum, Bounded)

outputProgromInsn :: Word -> Insn -> String
outputProgromInsn addr insn = 
  let asm = assemble insn
  in printf "// %s\n16'h%04x: data_ff <= 32'h%08x;\n" (show insn) addr asm

outputProgromObjBody :: Word -> Object -> String
outputProgromObjBody startAddr is = 
  concat $ map (uncurry outputProgromInsn) $ zip [startAddr ..] is

headerStr = 
  unlines ["`include \"../nkmm_const.v\""
          ,"module nkmm_progrom("
          ,"    input clk,"
          ,"    "
          ,"    output [`INSN_WIDTH-1:0] prog_data_o,"
          ,"    input [`ADDR_WIDTH-1:0] prog_addr_i);"
          ,""
          ,"reg [`INSN_WIDTH-1:0] data_ff;"
          ,""
          ,"always @(posedge clk) begin"
          ,"    case (prog_addr_i)"
          ,"// **** HEADER END ****"]

footerStr = 
  unlines ["// **** FOOTER BEGIN ****"
          ,"// Insn{M[--] D OpAdd(R0, imm 0)}"
          ,"default: data_ff <= 32'h10010000;"
          ,"    endcase"
          ,"end"
          ,""
          ,"assign prog_data_o = data_ff;"
          ,""
          ,"endmodule"]

serialize32Bes :: [Word32] -> BL.ByteString
serialize32Bes w32s = runPut $ mapM_ putWord32be w32s

chunk16 :: BL.ByteString -> [BL.ByteString]
chunk16 bs
    | BL.null bs = []
    | otherwise  = (BL.take 16 bs):(chunk16 (BL.drop 16 bs))

formatIntelHex32DataChunk :: BL.ByteString -> String
formatIntelHex32DataChunk c = startCode ++ hexStr (encoded `BL.snoc` (checksum encoded)) ++ "\n"
  where startCode :: String
        startCode = ":"

        byteCountP :: Int64 -> BL.ByteString
        byteCountP n | n <= 0xff = runPut $ putWord8 $ fromIntegral n

        addressP :: Int -> BL.ByteString
        addressP n | n <= 0xffff = runPut $ putWord16be $ fromIntegral n

        dataRecord :: Word8
        dataRecord = 0x00

        encoded :: BL.ByteString
        encoded = foldl1 BL.append
                      [byteCountP (BL.length c)
                      ,addressP (0xabcd)
                      ,BL.singleton dataRecord
                      ,c]

        checksum :: BL.ByteString -> Word8
        checksum bs = complement $ BL.foldl (+) (0x00 :: Word8) bs

        hexStr :: BL.ByteString -> String
        hexStr = BL.foldr f ""
          where f w s = (printf "%02x" w) ++ s


formatIntelHex32 :: BL.ByteString -> String
formatIntelHex32 bs = formattedChunks ++ eos
  where formattedChunks :: String
        formattedChunks = concatMap formatIntelHex32DataChunk $ chunk16 bs

        eos :: String
        eos = ":00000001FF\n"

outputObj :: OutputFormat -> Object -> String
outputObj Progrom obj = objBody -- headerStr ++ objBody ++ footerStr
  where objBody = outputProgromObjBody 0 obj

outputObj IntelHex obj = formatIntelHex32 bs
  where obj32 :: [Word32]
        obj32 = map assemble obj

        bs :: BL.ByteString
        bs = serialize32Bes obj32
