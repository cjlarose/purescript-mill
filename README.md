# purescript-mill

Early-stage attempt in PureScript to simulate the [Mill CPU][mill-home]. I intend to implement the functions for at least two [domains][domains] first, then move onto actually simulating [execution behavior][model] ([the belt][belt], [pipelines][pipelines], etc).

[mill-home]: http://millcomputing.com/
[domains]: http://millcomputing.com/wiki/Domains
[model]: http://millcomputing.com/topic/introduction-to-the-mill-cpu-programming-model-2/
[belt]: http://millcomputing.com/wiki/Belt
[pipelines]: http://millcomputing.com/wiki/Pipeline

For the unsigned integer domain, the following operations can be implemented (for scalars of all byte widths):

* [eqlu](http://millcomputing.com/wiki/Instruction_Set/eqlu)
* [gequ (lequ)](http://millcomputing.com/wiki/Instruction_Set/gequ)
* [gequ (lssu)](http://millcomputing.com/wiki/Instruction_Set/gtru)
* [widenu](http://millcomputing.com/wiki/Instruction_Set/widenu)
* [addu](http://millcomputing.com/wiki/Instruction_Set/addu)
* [mulu](http://millcomputing.com/wiki/Instruction_Set/mulu)
* [addus](http://millcomputing.com/wiki/Instruction_Set/addus)
* [mulus](http://millcomputing.com/wiki/Instruction_Set/mulus)
