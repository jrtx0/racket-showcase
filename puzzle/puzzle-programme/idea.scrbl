;idea.scrbl
;设计思路

#lang scribble/manual

@title[#:tag "idea"]{设计思路}

拼图本质上就是将一张完整的图片通过行列分割成相同大小的小块，然后打乱重排，并将重排后的图像展示给用户，有用户重新恢复拼合的过程。

由此，我们把游戏分为两大部分，一部分是将图片分割打乱的过程，这部分工作在后台完成；另一部分是将打乱重排的图片展示给用户，由用户重新拼合的交互过程，这部分通过图形界面和用户交互完成。

在Racket中是可以直接通过在交互栏内显示结果的，这个结果可以使文本内容，也可以是图形内容，这就给调试有图形输出的程序提供了极大的便利。

有了这个便利，我们可以先把有关图片操作的相关函数全部先写出来，然后把这些函数进行按需组合，就能实现了我们需要的程序——这就是自底向上的程序设计。然后我们设计好GUI中的相应功能界面，把相应的功能一个个和GUI功能界面连接，就完成了整个游戏了——这就是自顶向下的程序设计。

好了，我们先来看看底层需求有哪些？

@;
@include-section["idea/picture-control.scrbl"]
@include-section["idea/interface.scrbl"]
