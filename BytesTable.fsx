
module BytesTable =

    let getBytesTable (bytes : byte array) = 
        let maxLen = if bytes.LongLength > 1048576L then 1048576 else bytes.Length
        let sb = System.Text.StringBuilder ()
        bytes 
        |> Array.take maxLen
        |> Array.iteri (fun i c -> 
            if i % 16 = 0 then 
                if i > 0 then sb.AppendLine () |> ignore
                sb.AppendFormat("{0:x4}\t", i) |> ignore


            sb.AppendFormat("{0:x2}", c) |> ignore
            if i % 16 = 7 then sb.Append ' ' |> ignore
            elif i % 16 = 15 then
                sb.Append "    " |> ignore
                for i = i - 15 to i do
                    if bytes.[i] >= 32uy && bytes.[i] <= 126uy 
                    then bytes.[i] |> char
                    else '\183' 
                    |> sb.Append
                    |> ignore
            if i + 1 < maxLen then sb.Append ' ' |> ignore
        )

        sb.ToString ()    

