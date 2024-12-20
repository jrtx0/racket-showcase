;picture-control.scrbl
;图片操作功能

#lang scribble/manual

@title[#:tag "picture-control"]{图片操作功能}

具体每个功能对应哪个函数，后续程序设计部分可以看到，也可以对应源代码说明。

@itemlist[
 @item{取得源图片——从存储介质中读取图片；}
 @item{设置拼图图片尺寸——给定一个拼图图片大小的尺寸；
}
 @item{调整图片大小——读取的图片不一定是需要的尺寸,需要根据需要进行调整；}
@item{设置单元格行数列数——这个用来分割拼图图片，行列数越多拼起来越复杂；}
@item{调换单元格——将分割的图片中的指定两个单元格调换，这是打乱拼图的基础，对所有单元格进行调换就实现了打乱单元格的功能；}
@item{设置混合图片——将打乱混合的各个单元格图块组合成一张待拼图，这个用来展示给用户；}
@item{重置混合图片——考虑到用户可能觉得显示出来的拼图不够乱，可以再来打乱一次；}
@item{恢复混合图——考虑到用户拼到半途觉得没过瘾，需要再拼一次拼过的拼图，再给他一次机会；}
@item{绘制混合图片——把混合拼图绘制出来；}
@item{绘制目标图片——这个用来给用户一个完成的目标图片小样作为参照，可以提示或引导用户完成拼图操作；}
@item{根据坐标位置转id——因为我们使用鼠标操作，这里把鼠标光标位置转换成单元格图片的标志号（ID号。具体为什么要用ID号，下边专门做解释）；}
@item{绘制空白单元格——把单元格图块移走了，还没有填充图片的状态，就留下空白单元格，这是为了交互动画而需要的，具体原因下边解释；}
@item{在指定单元格位置绘制指定单元格内容——这个比较直白，就是在指定的单元格把指定的图块画上；}
@item{在指定坐标位置绘制指定单元格——把一个单元格的图块移到了其他单元格放上，就得在那个位置上把新的图块画出来；}
@item{绘制焦点单元格——如果鼠标移到某个单元格上，给出一个框做提示；}
@item{绘制失焦单元格——如果鼠标移出某个单元格,清除焦点提示;}
@item{取得当前单元格id——通过给定单元格行列号得到单元格ID号；}
@item{指定的id有效——判断给出的ID号是否属于拼图图块列表中的，防止误给出ID号；}
@item{根据拖动的坐标位置判断单元格id——当用鼠标按住一个单元格图块并拖动的时候，需要随时知道拖到哪个位置了，以便于后续的操作；}
@item{判断是否成功完成拼图——完成拼图了吗？这个需要随时检查判断一下，很多时候拼没拼对用户是无法判断的，因为图块之间太相似了；}
@item{初始化拼图数据——每个程序开始之前，难免要做些准备，这个就干这个的。}]

大致就是这些吧，如果有需要，后续还可以添加。

***解释两个问题，当然对于熟系的朋友来说并不一定需要。***

@itemlist[
@item{@bold{一、为什么要对每个图块标记ID号？}

这涉及到数据结构问题。数据结构我们在下边做一个简略说明，这里不做叙述，单就ID号的作用做一个说明。

必须的是，每个单元格图块都要唯一进行标记才能操作，这是没有疑问的。本来单元格图块直接用行号及列号就可以表示，也能够唯一标识，但是在用户拼图的时候，总会调来调去，位置一定会打乱的，这样我们就无法重新定位图块在原图上的位置（原图位置的一个用处是判断拼图是否完成），也不能定位刚打乱的时候的位置（为了恢复混合图，需要记住打乱时的位置），而且采用标志ID号也比较简单。}
 
@item{@bold{二、交互动画是如何实现的?}

大家应该都清楚我们看的影视动画都是在固定时间里通过显示静态图片实现的（比如每秒钟显示24帧）。

在我们程序的交互动画里边也可以是一帧一帧画面显示。比如鼠标每移动一次，画面刷新显示一次（根据鼠标移动情况随机发生）。可以这样来区分，于影视动画是固定时间变换帧动画，程序交互动画是随机变换帧动画。在交互动画中，很多时候我们实际只涉及图像里边的很小一块图像的变化，为了增加变换效率，减少每次变化计算机需要应对的数据量，可以把原交互图像作为背景，修改图像中变化的那一块图像，这样可提升交互操作的实时性。
}
]
