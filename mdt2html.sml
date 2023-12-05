fun reverse nil = nil | reverse (x::xs) = (reverse xs) @ [x];(*Function to
  reverse a given list*)
fun length nil = 0 | length (x::xs) = 1 + (length xs); (*Function to calculate
  the length of a given list*)  

fun trip_as(x:char list)=
  if length(x)<3 then false
  else
    let val y=hd(x)
        val z=hd(tl(x))
        val w=hd(tl(tl(x)))
    in
      if (y= #"*") andalso (z= #"*") andalso (w= #"*") then true
      else trip_as(tl(x))
    end;


fun bold_n_italics( x:char list , y1:char list, z1:bool)=
  if length(x)<6 then reverse(y1)@x
  else
    let val y=hd(x)
        val z=hd(tl(x))
        val w=hd(tl(tl(x)))
    in 
      if z1=true andalso (y= #"*") andalso (z= #"*") andalso (w= #"*") then bold_n_italics(tl(tl(tl(x))),reverse(explode("</i></b>"))@y1,false)
      else if z1=false andalso (y= #"*") andalso (z= #"*") andalso (w= #"*") andalso trip_as(tl(tl(tl(x)))) then bold_n_italics(tl(tl(tl(x))),reverse(explode("<b><i>"))@y1,true)
    else bold_n_italics(tl(x),[hd(x)]@y1,z1)
    end;

fun bold_italics(x:char list)= bold_n_italics(x,[],false);

fun double_as(x:char list)=
  if length(x)<2 then false
  else
    let val y=hd(x)
        val z=hd(tl(x))
    in
      if (y= #"*") andalso (z= #"*") then true
      else double_as(tl(x))
    end;

fun bold(x: char list,y1:char list,z1:bool)=
  if length(x)<4 then reverse(y1)@x
  else
    let val y=hd(x)
        val z=hd(tl(x))
    in
      if z1=true andalso (y= #"*") andalso (z= #"*") then bold(tl(tl(x)),reverse(explode("</b>"))@y1,false)
      else if z1=false andalso (y= #"*") andalso (z= #"*") andalso double_as(tl(tl(x))) then bold(tl(tl(x)),reverse(explode("<b>"))@y1,true)
      else bold(tl(x),[hd(x)]@y1,z1)
    end;

fun bold1(x:char list)= bold(x,[],false);

fun single_as(x:char list)=
  if length(x)<1 then false
  else
    let val y=hd(x)
    in
      if (y= #"*") then true
      else single_as(tl(x))
    end;
fun italic(x:char list,y1:char list,z1: bool)=
  if length(x)<2 then reverse(y1)@x
  else
    let val y=hd(x)
    in
      if z1=true andalso (y= #"*") then italic(tl(x),reverse(explode("</i>"))@y1,false)
      else if z1=false andalso (y= #"*") andalso single_as(tl(x)) then italic(tl(x),reverse(explode("<i>"))@y1,true)
      else italic(tl(x),[hd(x)]@y1,z1)
    end;

fun italic1(x:char list)= italic(x,[],false);

fun ul_helper(x:char list)=
  if length(x)<1 then false
  else
    let val y=hd(x)
    in
      if (y= #" ") then false
      else if (y= #"_") then true
      else ul_helper(tl(x))
    end;

fun underline(x:char list,y1:char list,z1:bool)=
  if length(x)<2 then reverse(y1)@x
  else
    let val y=hd(x)
    in
      if z1=true andalso (y= #"_") then 
        if ul_helper(tl(x)) then underline(tl(x),reverse(explode(" "))@y1,true)
        else underline(tl(x),reverse(explode("</u>"))@y1,false)
      else if z1=false andalso (y= #"_") andalso ul_helper(tl(x)) then underline(tl(x),reverse(explode("<u>"))@y1,true)
      else underline(tl(x),[y]@y1,z1)
    end;

fun ul_1(x:char list)= underline(x,[],false);

fun hash_detection(x:char list , y: char list, z: bool,count:int)=
  if length(x)<1 then reverse(y)@x
  else 
    let val a=hd(x)
    in
      if (((a= #">") orelse (a= #" ")) andalso z=false) then hash_detection(tl(x),[a]@y,z,count)
      else if (a= #"#") andalso count<6 then hash_detection(tl(x),y,true,count+1)
      else if z=true andalso ((a<> #"#") orelse count=6) then reverse(y)@explode("<h"^(Int.toString(count))^">")@x@explode("</h"^(Int.toString(count))^">")
      else hash_detection(tl(x),[a]@y,z,count)
      end;    

fun hash1(x:char list)= hash_detection(x,[],false,0);

fun horizontal_ruling_valid(x:char list,blkquot: bool,spaces:bool ,count:int)=
  if length(x)<1 andalso count<3 then false
  else if length(x)<1 andalso count>=3 then true
  else
    let val a=hd(x)
    in
      if a= #">" andalso blkquot=true then horizontal_ruling_valid(tl(x),true,spaces,count)
      else if a= #" " andalso blkquot=true then horizontal_ruling_valid(tl(x),false,spaces,count)
      else if a= #" " andalso blkquot=false andalso spaces=true then horizontal_ruling_valid(tl(x),false,true,count)
      else if a<> #" " andalso a<> #"-" andalso spaces=true then false
      else if a= #"-" then  horizontal_ruling_valid(tl(x),false,false,count+1)
      else false
    end;

fun hr_helper(x:char list,y:char list)=
  let val a=hd(x)
  in
    if a= #">" orelse a= #" " then hr_helper(tl(x),[a]@y)
    else reverse(y)@explode("<hr />")
  end;
fun hr(x:char list)=
  if horizontal_ruling_valid(x,true,true,0) then hr_helper(x,[])
  else x;

fun valid_block( x : char list,count:int , flag:bool)=
    if x=[] then ( false, count )
    else
    let 
        val head = hd(x)
    in
        if head = #">" andalso flag=true then valid_block(tl(x),count+1,true)
        else if head = #" " then valid_block(tl(x),count,false)
        else if head = #"\n" then ( false, count )
        else ( true, count )
    end;

fun valid_blk(x:char list)= valid_block(x,0,true);
fun empty_line( x: char list)=
    if x=[] then true
    else
    let
        val head = hd(x)
    in
        if head = #" " then empty_line(tl(x))
        else if head = #"\n" then true
        else false
    end;

fun block_list( x:char list , y:int)=
    if y=0 then x
    else block_list(tl(x),y-1);

fun block_gen( x: string, y:int)=
    if y=0 then (x)
    else block_gen("<blockquote>"^x,y-1);
fun block_gen_2( x: string, y:int)=
    if y=0 then (x)
    else block_gen_2("</blockquote>"^x,y-1);
fun counting_blocks(x:char list, suffix: char list, prev_count: int, running_p:bool)=
    let
      val valid= valid_blk(x)
      val validity= #1valid
      val curr_count= #2valid
      val x = block_list(x,curr_count)
    in 
      if empty_line(x) then 
        if running_p=true then (implode(x)^"</p>"^implode(suffix),0,false,[])
        else (implode(x)^implode(suffix),0,false,[])
      else if validity=false andalso running_p=false then ("<p>"^implode(x),prev_count,true,suffix)
      else if validity=false andalso running_p=true then (implode(x),prev_count,true,suffix)
      else if validity=true andalso curr_count> prev_count then 
        if running_p then ("</p>"^block_gen("",curr_count-prev_count)^"<p>"^implode(x),curr_count,true,explode(block_gen("",curr_count-prev_count))@suffix)
        else (block_gen("",curr_count-prev_count)^"<p>"^implode(x),curr_count,true,explode(block_gen("",curr_count-prev_count))@suffix)
      else (implode(x),prev_count,true,suffix)
    end;

fun counting_blocks_2(x:char list, suffix: char list,prev_count:int, running_p:bool)=
    let
      val valid= valid_blk(x)
      val validity= #1valid
      val curr_count= #2valid
      val x = block_list(x,curr_count)
    in 
      if empty_line(x) then 
        if running_p=true then (implode(x)^"</p>"^implode(suffix),0,false,[])
        else (implode(x)^implode(suffix),0,false,[])
      else if validity=false andalso running_p=false then (implode(suffix)^"<p>"^implode(x),0,true,[])
      else if validity=false andalso running_p=true then (implode(suffix)^implode(x),0,true,[])
      else if validity=true andalso curr_count> prev_count then 
        if running_p then ("</p>"^block_gen("",curr_count-prev_count)^"<p>"^implode(x),curr_count,true,explode(block_gen_2("",curr_count-prev_count))@suffix)
        else (block_gen("",curr_count-prev_count)^"<p>"^implode(x),curr_count,true,explode(block_gen_2("",curr_count-prev_count))@suffix)
      else ("</p>"^implode(suffix)^block_gen("",curr_count)^"<p>"^implode(x),curr_count,true,explode(block_gen_2("",curr_count)))
    end;

fun email_block_valid(x:char list,count:int)=
  if x=[] then count
  else
    let 
      val head=hd(x)
    in
      if head= #">" then email_block_valid(tl(x),count+1)
      else count
    end;

fun blk_count(x:char list)= email_block_valid(x,0);

fun email_block_gen(xi:char list,prev_count:int, running_p:bool)=
  let 
    val indent= blk_count(xi)
    val x = block_list(xi,indent)
  in 
    if empty_line(x) then if running_p=true then ("</p>"^block_gen_2("",prev_count),0,false) else (""^block_gen_2("",prev_count),0,false)
    else if indent=0 then
     if running_p= true andalso prev_count= 0 then (implode(x),0,true)
     else  if running_p= true andalso prev_count> 0 then ("</p"^block_gen_2("",prev_count)^"<p>"^implode(x),0,true)
     else if running_p= false andalso prev_count= 0 then ("<p>"^implode(x),0,true)
     else (block_gen_2("",prev_count)^"<p>"^implode(x),0,true)
    else if indent> prev_count then 
      if running_p= true then ("</p>"^block_gen("",indent-prev_count)^"<p>"^implode(x),indent,true)
      else (block_gen("",indent-prev_count)^"<p>"^implode(x),indent,true)
    else if indent< prev_count then 
      if running_p= true then ("</p>"^block_gen_2("",prev_count)^block_gen("",indent)^"<p>"^implode(x),indent,true)
      else (block_gen_2("",prev_count)^block_gen("",indent)^"<p>"^implode(x),indent,true)
    else 
      if running_p= true then ("</p>"^"<p>"^implode(x),indent,true)
      else ("<p>"^implode(x),indent,true)

  end;

fun link_extractor(x:char list,y:char list, read:bool)=
  if length(x)<1 then reverse(y)
  else 
    let
      val head= hd(x)
    in
      if head= #"]" then link_extractor(tl(x),y,true)
      else if read=true then
        if head= #" " then reverse(y)
        else link_extractor(tl(x),head::y,true)
      else link_extractor(tl(x),y,read)
    end;

fun word_extractor(x:char list,y:char list)=
if length(x)<1 then reverse(y)
else
  let
    val head= hd(x)
  in 
    if head= #"]" then reverse(y)
    else word_extractor(tl(x),head::y)
  end;

fun resume(x:char list, y: int,z:bool)=
  if length(x)<1 then []
  else if y= 0 then x
  else if z=true then
    let
      val head= hd(x)
    in
      if head= #" " then resume(tl(x),y-1,z)
      else resume (tl(x),y,z)
    end
  else 
    let
      val head= hd(x)
    in
      if head= #"]" then resume(tl(x),y,true)
      else resume (tl(x),y,z)
    end;

fun link(x:char list, y1:char list)=
  if length(x)<1 then reverse(y1)
  else
    let
      val head= hd(x)
      val body= "<a href=\"" ^ implode(link_extractor(x,[],false)) ^ "\">" ^ implode(word_extractor(tl(x),[])) ^ "</a>"
    in 
      if head= #"[" then link( resume(x,1,false), reverse(explode(body))@y1)
      else link(tl(x),head::y1)
    end;


fun table_helper(x:char list,y:int)=
  if length(x)<2 then 0
  else
    let
      val a=hd(x)
      val b=hd(tl(x))
    in
      if y=0 then
        if a= #"<" andalso b= #"<" then 1
        else 0
      else if y=1 then
        if a= #">" andalso b= #">" then 2
        else 3
      else if y=2 then
        if a= #"<" andalso b= #"<" then 1
        else 0
      else if y=3 then
        if a= #">" andalso b= #">" then 2
        else 3
      else 0
    end;


fun table_gen(x:char list,y1:char list,t_start:int)=
  if length(x)<1 then explode("<TR><TD>")@reverse(y1)@explode("</TD></TR>")@x
  else
    if t_start= 1 then (explode("<CENTER><TABLE border=\"1\">"))
    else if t_start= 2 then (explode("</TABLE></CENTER>"))
    else if t_start= 3 then 
      let 
        val a=hd(x)
      in
        if a= #"|" then table_gen(tl(x),reverse(explode("</TD><TD>"))@y1,t_start)
        else table_gen(tl(x),a::y1,t_start)
      end
    else x


fun mdt2html(x: string)=
  let 
  val iptxt=TextIO.openIn(x)
  val ophtml=TextIO.openOut(implode(reverse(reverse(explode("html"))@tl(tl(tl(reverse(explode(x)))))))) 
  fun main( continew : bool, prev_count: int,running_p:bool,t_start:int )=
    let
    val check=TextIO.endOfStream(iptxt)
    val newline=TextIO.inputLine(iptxt)
    val currline = if check then " " else valOf(newline)
    val outputline0 = bold_italics(explode(currline))
    val outputline1 = link(outputline0,[])
    val outputline2 = bold1(outputline1)
    val outputline3 = italic1(outputline2)
    val outputline4 = ul_1(outputline3)
    val outputline5 = table_gen(outputline4,[],table_helper(outputline4,t_start))
    val outputline6 = hash1(outputline5)
    val outputline7 = hr(outputline6)
    val outputline8 = email_block_gen(outputline7,prev_count,running_p)
    in
    if check
    then
    if running_p then "</p>"^block_gen_2("",prev_count)
    else 
    " "^block_gen_2("",prev_count)
    else
    #1outputline8^main(false,#2outputline8,#3outputline8,table_helper(outputline4,t_start))
    end;
  val output_write= TextIO.output(ophtml,main(false,0,false,0))
  val output_close= TextIO.closeOut(ophtml)
  in 
    1
  end;  
