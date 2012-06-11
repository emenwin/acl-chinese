.. highlight:: cl
   :linenothreshold: 0

Chapter 11 Common Lisp 对象系统 CLOS
**************************************************
Common Lisp 的对象系统(Object System)，或者说CLOS, 是指用于面向对象编程的一组操作符的集合。由于它们共同的历史，
方便起见，我们会把它们当成一组操作符来看待。技术上讲，这些操作符和 Common Lisp 的其他功能并没有什么不同：
defmethod 和 defun 一样，只不过是语言的一个普通组成部分。

11.1 Object-Oriented Programming
======================================
面向对象的编程意味着程序组织方式的改变。这种改变可以和当初处理器运算能力的分布式发展相提并论。在1970年，
一台多用户的计算机系统意味着一到两台巨大的大型机和很多粗笨的终端相连。现在，很多工作站通过网络互相连接就能实现同样的功能。
处理器的运算能力分布到了每个用户手中，而不是集中的由一个大型计算机所独占。

面向对象编程以类似的方式对传统的程序进行拆分。在面向对象编程中，不是通过单一的程序对大量不活泼的数据进行操作，
相反，程序会告诉数据自身该如何去运作，而且，对于这些数据“对象”之间该如何互动，程序是不明确指定的。

比如，我们要写个程序来计算二维形状的面积。一个可行的实现的方法是，写一个简单的函数，判断参数的类型，并按情况
实现相应的功能，如图 11.1 所示。::

   (defstruct rectangle
      height width)

   (defstruct circle
      radius)

   (defun area (x)
      (cond ((rectangle-p x)
               (* (rectangle-height x) (rectangle-width x)))
            ((circle-p x)
               (* pi (expt (circle-radius x) 2)))))

   > (let ((r (make-rectangle)))
      (setf (rectangle-height r) 2
            (rectangle-height r) 3)
      (area r))

   6

（图11.1：用结构体和函数实现求面积功能）

使用 CLOS 我们可以写一个等价的程序，如图11.2. ::

   (defclass rectangle ()
      (height width))

   (defclass circle ()
      radius))

   (defmethod area ((x rectangle))
      (* (slot-value x 'height) (slot-value x 'width)))

   (defmethod area ((x circle))
      (* pi (expt (slot-value x 'radius) 2)))

   > (let ((r (make-instance 'rectangle)))
      (setf (slot-value r 'height) 2   
            (slot-value r 'width) 3)
         (area r))

(图11.2：用类和方法实现的求面积）

在面向对象的模型中，程序的代码被打散到了多个独立的方法中，
每一个方法可以接受一些参数。图11.2中定义的两个方法隐式的定义了 area 函数，其功能和图11.1中所示的完全相同。
当我们调用 area 时，Lisp 会查看参数的类型，并自动调用与之对应的方法。

在面向对象的编程中，不但函数的功能被打散到了一个个独立的方法中，而且，面向对象编程还意味着*继承*—— 适用于
槽（slots） 和方法（methods）。图11.2中，defclass 的第二个参数——空列表表示一组父类的列表。假设我们需要定义
一类有颜色的物体，然后再定义一类有颜色的圆形，它以 colored 和 circle 作为父类：::

   (defclass colored ()
      (color))

   (defclass colored-circle (circle colored)
      ())

当我们创建 colored-circle 的实例时，我们会看到两种继承：

1. colored-circle 的实例会拥有2个槽：radius (从 circle 类继承而来），以及 color（从 colored 类继承而来）。
2. 因为没有为 colored-circle 类的实例显式定义 area 方法，如果我们对 colored-circle 的实例调用 area 方法，
   我们会调用到定义在 circle 类上的方法。

用实践术语来讲，面向对象的编程意味着，使用方法，类，实例，继承来组织程序。为什么我们要这样来组织程序呢？面向对象编程
宣称的一个优势是，这样会使得程序更容易修改。如果我们想要修改 ob 类的实例显示的方式，只要修改 ob 类的 display 方法即可。
如果我们要创建一类新的对象，他们和 ob 相似，但在少数有些方面有所不同，我们可以创建 ob 的子类；在子类中，我们
按照我们的需求修改属性，而其他特征则会默认的从 ob 类继承下来。而如果我们仅仅想要某一个 ob 对象和其他 ob 对象的行为
有所不同，我们可以创建 ob 的一个新子类，并直接修改它的属性。如果整个程序从一开始就仔细编写，我们甚至可以在不看其他
代码的情况下做出以上提到的所有这些类型的修改。



11.2 Class and Instances
==================================================

11.3 Slot Properties
================================

11.4 Superclasses
===================================================

11.5 Precedence
=======================================

11.6 Generic Functions
=======================================

11.7 Auxiliary Methods
==================================================

11.8 Method Combination
=======================================

11.9 Encapsulation
===================================

11.10 Two Models
========================================

Chapter 11 总结 (Summary)
============================

Chapter 11 练习 (Exercises)
==================================