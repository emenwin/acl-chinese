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

比如，我们要写个程序来计算二维形状的面积。一个可行的实现的方法是，写一个简单的函数，判断参数的类型，并按不同情况
分别实现相应的功能，如图 11.1 所示。::

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
            (rectangle-width r) 3)
      (area r))

   6

（图11.1：用结构体和函数实现求面积功能）

使用 CLOS 我们可以写一个等价的程序，如图11.2. ::

   (defclass rectangle ()
      (height width))

   (defclass circle ()
      (radius))

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

在面向对象的编程中，不但函数的功能被打散到了一个个独立的方法中，而且，面向对象编程还意味着 *继承* —— 对象的
槽（slots） 和方法（methods）都支持继承。图11.2中，defclass 的第二个参数——空列表——表示一组父类的列表。假设我们需要定义
一类彩色的物体，然后再定义一类彩色的圆形，它以 colored 和 circle 作为父类：::

   (defclass colored ()
      (color))

   (defclass colored-circle (circle colored)
      ())

当我们创建 colored-circle 的实例时，我们会看到两种继承：

1. colored-circle 的实例会拥有2个 slots：radius (从 circle 类继承而来），以及 color（从 colored 类继承而来）。
2. 因为没有为 colored-circle 类的实例显式定义 area 方法，如果我们对 colored-circle 的实例调用 area 方法，
   我们会调用到定义在 circle 类上的方法。

用实践术语来讲，面向对象的编程意味着使用方法、类、实例、继承来组织程序。为什么要这样组织程序呢？面向对象编程
宣称的一个优势是，这样会使得程序更容易修改。如果我们想要修改 ob 类的实例显示的方式，只要修改 ob 类的 display 方法即可。
如果我们要创建一类新的对象，他们和 ob 相似，但在少数有些方面有所不同，那我们可以创建 ob 的子类；在子类中，我们
按照需要修改属性，而其他特征则会默认的从 ob 类继承下来。而如果我们仅仅想要某一个 ob 对象和其他 ob 对象的行为
有所不同，我们可以创建 ob 的一个新子类，并直接修改它的属性。如果整个程序从一开始就仔细编写，我们甚至可以在不看其他
代码的情况下进行上述这些修改。



11.2 类和实例(Class and Instances)
==================================================
在4.6节中，我们学到了创建 structure 的两个步骤：首先我们调用 defstruct 来定义结构体的形式，然后通过一个特定的函数，
比如 make-point, 来创建它。创建实例同样需要两个类似的步骤。首先我们定义一个类(class), 用 defclass:::

(defclass circle ()
   (radius center))

这个定义表示，circle 类的实例将会包含2个 slots （就像 structure 中的 fields），分别命名为 radius 和 center.
要创建这个类的实例，我们不是通过调用特定的函数，而是调用一个通用的 make-instance 函数来实现，第一个参数设置成要创建的类的名字：::

> (setf c (make-instance 'circle))
#<Circle #XC27496>

要给这个实例的 slots 赋值，我们可以用 setf:::

> (setf (slot-value c 'radius) 1)
1

和 structure 的 fields 一样，如果 slots 没有初始化，它们的值为 undefined.

11.3 Slot 的属性(Slot Properties)
================================
defclass 的第三个参数必须是包含着一组 slot 定义的列表。如上例所示，最简单的 slot 定义形式是一个 symbol, 表示
slot 的名字。在一般情况下，slot 的定义形式可以是一个列表，其第一个元素为名称，接下来的是一些属性，而属性的定义
形式和关键字参数类似。

通过 :accessor 来定义一个 slot, 我们隐式的定义了一个访问那个 slot 的函数，这样，就不需要调用 slot-value 了。
如果我们将 circle 类的定义修改如下：::

(defclass circle ()
   ((radius :accessor circle-radius)
    (center :accessor circle-center)))

那我们就可以通过 circle-radius 和 circle-center 来引用 circle 类的 slots:::

> (setf c (make-instance 'circle))
#<Circle #XC5C726>

> (setf circle-radius c) 1)
1
> (circle-radius c)
1

还可以指定 :writer 或 :reader, 而不是 :accessor, 这样我们可以仅得到上面定义的一半的行为（只读或者仅仅可写）。

要指定 slot 的默认值，我们必须用 :initform 参数。如果我们想在 make-instance 的调用中初始化 slot, 我们可以
通过 :initarg 定义一个参数名。我们的类定义现在可以改成如下形式：::

(defclass circle ()
   ((radius :accessor circle-radius
            :initarg :radius
            :initform 1)
   (center  :accessor circle-center
            :initarg :center
            :initform (cons 0 0))))

现在，我们在创建 circle 类的实例时，既可以通过上面通过 :initarg 定义的关键字参数名来给 slot 赋值，也可以
让 slot 的 value 保持 :initform 中定义的默认值。::

> (setf c (make-instance 'circle :radius 3))
#<Circle #XC2DE0E>
> (circle-radius c)
3
> (circle-center c)
(0 . 0)

注意，:initargs 比 :initform 的优先级要高。

我们可以指定某些 slots 的值是共享的——也就是说，他们的值对每一个类实例而言都是相同的。实现的方式是在 slot 的定义中
加上 :allocation :class. (另一个可选值是 :allocation :instance, 但因为这是默认值，所以不需要显式指明）。
如果我们在一个实例中修改这种 slot 的值，其他所有 instances 中的该 slot 也会获得同样的值。所以我们要用共享的 slots 来
保存所有实例的共性。

例如，假设我们要模拟一组小报(tabloids)的行为。在我们的模拟中，我们想要表示下列事实：当某一个 tabloid 报道某个主题时，其他的也一样。
我们可以通过让所有实例共享一个 slot 的方式来实现。tabloid 类可定义如下：::

(defclass tabloid ()
   ((top-story :accessor tabloid-story
               :allocation :class)))

现在，如果我们创建两个 tabloids, 如果某个报道在其中任一个 tabloid 上成为首页新闻，那么它也会同时成为另一个 tabloid 的首页新闻：::

> (setf daily-blab (make-instance 'tabloid)
      unsolicited-mail (make-instance 'tabloid))
#<Tabloid #XC2AB16>
> (setf (tabloid-story (daily-blab) 'adultery-of-senator)
ADULTERY-OF-SENATOR
> (tabloid-story unsolicited-mail)
ADULTERY-OF-SENATOR

另外，如果指定了 :documentation 属性，则可以用来表示该 slot 的文档说明。而 :type 则可以用来指定该 slot 中元素的类型。
类型的声明将在 13.3 节中讲解。

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