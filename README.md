# purescript-mill

Early-stage attempt in PureScript to simulate the [Mill CPU][mill-home]. I intend to implement the functions for at least two [domains][domains] first, then move onto actually simulating [execution behavior][model] ([the belt][belt], [pipelines][pipelines], etc).

[mill-home]: http://millcomputing.com/
[domains]: http://millcomputing.com/wiki/Domains
[model]: http://millcomputing.com/topic/introduction-to-the-mill-cpu-programming-model-2/
[belt]: http://millcomputing.com/wiki/Belt
[pipelines]: http://millcomputing.com/wiki/Pipeline

For the unsigned integer domain, the following operations can be implemented (for scalars of all byte widths):

* [x] [widenu](http://millcomputing.com/wiki/Instruction_Set/widenu)
* Comparison
  * [x] [eqlu](http://millcomputing.com/wiki/Instruction_Set/eqlu)
  * [x] [nequ](http://millcomputing.com/wiki/Instruction_Set/nequ)
  * [x] [gequ (lequ)](http://millcomputing.com/wiki/Instruction_Set/gequ)
  * [x] [gtru (lssu)](http://millcomputing.com/wiki/Instruction_Set/gtru)
* Bitwise
  * [ ] [nandu](http://millcomputing.com/wiki/Instruction_Set/nandu)
  * [ ] [orlu](http://millcomputing.com/wiki/Instruction_Set/orlu)
  * [ ] [noru](http://millcomputing.com/wiki/Instruction_Set/noru)
  * [ ] [xorlu](http://millcomputing.com/wiki/Instruction_Set/xorlu)
  * [ ] [nxoru](http://millcomputing.com/wiki/Instruction_Set/nxoru)
  * [ ] [impu](http://millcomputing.com/wiki/Instruction_Set/impu)
  * [ ] [nimpu](http://millcomputing.com/wiki/Instruction_Set/nimpu)
  * [ ] [shiftru](http://millcomputing.com/wiki/Instruction_Set/shiftru)
  * [ ] [flipu](http://millcomputing.com/wiki/Instruction_Set/flipu)
  * [ ] [notlu](http://millcomputing.com/wiki/Instruction_Set/notlu)
* Arithmetic
  * [ ] [divRemu](http://millcomputing.com/wiki/Instruction_Set/divRemu)
  * [ ] [divu](http://millcomputing.com/wiki/Instruction_Set/divu)
  * [ ] [rdivu](http://millcomputing.com/wiki/Instruction_Set/rdivu)
  * [ ] [remu](http://millcomputing.com/wiki/Instruction_Set/remu)
  * [ ] [rootu](http://millcomputing.com/wiki/Instruction_Set/rootu)
  * [ ] [rrootu](http://millcomputing.com/wiki/Instruction_Set/rrootu)
  * Modulo
    * [ ] [add1u](http://millcomputing.com/wiki/Instruction_Set/add1u)
    * [ ] [sub1u](http://millcomputing.com/wiki/Instruction_Set/sub1u)
    * [x] [addu](http://millcomputing.com/wiki/Instruction_Set/addu)
    * [ ] [subus](http://millcomputing.com/wiki/Instruction_Set/subus)
    * [x] [mulu](http://millcomputing.com/wiki/Instruction_Set/mulu)
    * [x] [narrowu](http://millcomputing.com/wiki/Instruction_Set/narrowu)
    * [ ] [shiftlu](http://millcomputing.com/wiki/Instruction_Set/shiftlu)
  * Saturating
    * [x] [addus](http://millcomputing.com/wiki/Instruction_Set/addus)
    * [ ] [subus](http://millcomputing.com/wiki/Instruction_Set/subus)
    * [x] [mulus](http://millcomputing.com/wiki/Instruction_Set/mulus)
    * [ ] [narrowus](http://millcomputing.com/wiki/Instruction_Set/narrowus)
    * [ ] [shiftlus](http://millcomputing.com/wiki/Instruction_Set/shiftlus)
  * Excepting
  * Widening
