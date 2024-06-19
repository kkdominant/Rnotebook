
# 一、R语言基础知识


## 数据的输入、数据的类型及简单计算

``` r
weight <- c(90, 85, 95, 92, 88)
typeof(weight)
```

‘double’

> 在 R 语言中，double是一种用于表示双精度浮点数的数据类型。双精度浮点数可以存储十进制数，并具有较高的精度和范围，用于表示和计算实数。double
> 类型是 R 中数值数据的默认类型。

``` r
weight_1 <- as.character(weight);typeof(weight_1) # 转换为characte(字符)类型
weight_2 <- as.numeric(weight_1);typeof((weight_2)) # 转换为numeric(数值)类型
```

‘character’

‘double’

``` r

age <- c(21, 43, 52, 32, 43)
```

``` r
mergedata = c(weight, age) #合并两组数据
```

``` r
mean(age) # 计算年龄均值
```

38.2

``` r
sd(age) # 计算年龄标准差
```

11.9457105272144

``` r
var(age) # 计算年龄方差
var(mergedata) 
```

142.7

815.211111111111

``` r
cov(age, weight) # 计算年龄与体重的协方差
var(age, weight) #var(x,y)=cov(x,y)
```

5.75

5.75

``` r
cor(age,weight,method="pearson") # 计算年龄与体重的相关系数
```

0.126407213237481

``` r
plot(weight,age) # 画出年龄与体重的散点图
```
![](https://kkdominant.oss-cn-shanghai.aliyuncs.com/img/202406191556848.png)

使用`example(function)`查看某函数的使用方法

``` r
example(cov2cor)
```
{% hideToggle 输出内容过多点击展开 %}

    cov2cr> var(1:10)  # 9.166667
    [1] 9.166667

    cov2cr> var(1:5, 1:5) # 2.5
    [1] 2.5

    cov2cr> ## Two simple vectors
    cov2cr> cor(1:10, 2:11) # == 1
    [1] 1

    cov2cr> ## Correlation Matrix of Multivariate sample:
    cov2cr> (Cl <- cor(longley))
                 GNP.deflator       GNP Unemployed Armed.Forces Population
    GNP.deflator    1.0000000 0.9915892  0.6206334    0.4647442  0.9791634
    GNP             0.9915892 1.0000000  0.6042609    0.4464368  0.9910901
    Unemployed      0.6206334 0.6042609  1.0000000   -0.1774206  0.6865515
    Armed.Forces    0.4647442 0.4464368 -0.1774206    1.0000000  0.3644163
    Population      0.9791634 0.9910901  0.6865515    0.3644163  1.0000000
    Year            0.9911492 0.9952735  0.6682566    0.4172451  0.9939528
    Employed        0.9708985 0.9835516  0.5024981    0.4573074  0.9603906
                      Year  Employed
    GNP.deflator 0.9911492 0.9708985
    GNP          0.9952735 0.9835516
    Unemployed   0.6682566 0.5024981
    Armed.Forces 0.4172451 0.4573074
    Population   0.9939528 0.9603906
    Year         1.0000000 0.9713295
    Employed     0.9713295 1.0000000

    cov2cr> ## Graphical Correlation Matrix:
    cov2cr> symnum(Cl) # highly correlated
                 GNP. GNP U A P Y E
    GNP.deflator 1                 
    GNP          B    1            
    Unemployed   ,    ,   1        
    Armed.Forces .    .     1      
    Population   B    B   , . 1    
    Year         B    B   , . B 1  
    Employed     B    B   . . B B 1
    attr(,"legend")
    [1] 0 ' ' 0.3 '.' 0.6 ',' 0.8 '+' 0.9 '*' 0.95 'B' 1

    cov2cr> ## Spearman's rho  and  Kendall's tau
    cov2cr> symnum(clS <- cor(longley, method = "spearman"))
                 GNP. GNP U A P Y E
    GNP.deflator 1                 
    GNP          B    1            
    Unemployed   ,    ,   1        
    Armed.Forces          . 1      
    Population   B    B   ,   1    
    Year         B    B   ,   1 1  
    Employed     B    B   .   B B 1
    attr(,"legend")
    [1] 0 ' ' 0.3 '.' 0.6 ',' 0.8 '+' 0.9 '*' 0.95 'B' 1

    cov2cr> symnum(clK <- cor(longley, method = "kendall"))
                 GNP. GNP U A P Y E
    GNP.deflator 1                 
    GNP          B    1            
    Unemployed   .    .   1        
    Armed.Forces            1      
    Population   B    B   .   1    
    Year         B    B   .   1 1  
    Employed     *    *   .   + + 1
    attr(,"legend")
    [1] 0 ' ' 0.3 '.' 0.6 ',' 0.8 '+' 0.9 '*' 0.95 'B' 1

    cov2cr> ## How much do they differ?
    cov2cr> i <- lower.tri(Cl)

    cov2cr> cor(cbind(P = Cl[i], S = clS[i], K = clK[i]))
              P         S         K
    P 1.0000000 0.9802390 0.9572562
    S 0.9802390 1.0000000 0.9742171
    K 0.9572562 0.9742171 1.0000000

    cov2cr> ## cov2cor() scales a covariance matrix by its diagonal
    cov2cr> ##           to become the correlation matrix.
    cov2cr> cov2cor # see the function definition {and learn ..}
    function (V) 
    {
        p <- (d <- dim(V))[1L]
        if (!is.numeric(V) || length(d) != 2L || p != d[2L]) 
            stop("'V' is not a square numeric matrix")
        pos <- !is.na(Is <- D <- diag(V, names = FALSE)) & D > 0
        Is[pos] <- sqrt(1/D[pos])
        Is[!pos] <- NaN
        if (any(!pos) || any(!is.finite(Is))) 
            warning("diag(V) had non-positive or NA entries; the non-finite result may be dubious")
        r <- V
        r[] <- Is * V * rep(Is, each = p)
        if (p) 
            r[seq.int(from = 1L, by = p + 1L, length.out = p)] <- 1
        r
    }
    <bytecode: 0x000001d23fd06730>
    <environment: namespace:stats>

    cov2cr> stopifnot(all.equal(Cl, cov2cor(cov(longley))),
    cov2cr+           all.equal(cor(longley, method = "kendall"),
    cov2cr+             cov2cor(cov(longley, method = "kendall"))))

    cov2cr> ##--- Missing value treatment:
    cov2cr> C1 <- cov(swiss)

    cov2cr> range(eigen(C1, only.values = TRUE)$values) # 6.19        1921
    [1]    6.191249 1921.562488

    cov2cr> ## swM := "swiss" with  3 "missing"s :
    cov2cr> swM <- swiss

    cov2cr> colnames(swM) <- abbreviate(colnames(swiss), minlength=6)

    cov2cr> swM[1,2] <- swM[7,3] <- swM[25,5] <- NA # create 3 "missing"

    cov2cr> ## Consider all 5 "use" cases :
    cov2cr> (C. <- cov(swM)) # use="everything"  quite a few NA's in cov.matrix
              Frtlty Agrclt Exmntn     Eductn Cathlc    Infn.M
    Frtlty 156.04250     NA     NA -79.729510     NA 15.156193
    Agrclt        NA     NA     NA         NA     NA        NA
    Exmntn        NA     NA     NA         NA     NA        NA
    Eductn -79.72951     NA     NA  92.456059     NA -2.781684
    Cathlc        NA     NA     NA         NA     NA        NA
    Infn.M  15.15619     NA     NA  -2.781684     NA  8.483802

    cov2cr> try(cov(swM, use = "all")) # Error: missing obs...
    Error in cov(swM, use = "all") : missing observations in cov/cor

    cov2cr> C2 <- cov(swM, use = "complete")

    cov2cr> stopifnot(identical(C2, cov(swM, use = "na.or.complete")))

    cov2cr> range(eigen(C2, only.values = TRUE)$values) # 6.46   1930
    [1]    6.462385 1930.505982

    cov2cr> C3 <- cov(swM, use = "pairwise")

    cov2cr> range(eigen(C3, only.values = TRUE)$values) # 6.19   1938
    [1]    6.194469 1938.033663

    cov2cr> ## Kendall's tau doesn't change much:
    cov2cr> symnum(Rc <- cor(swM, method = "kendall", use = "complete"))
           F A Ex Ed C I
    Frtlty 1            
    Agrclt   1          
    Exmntn . . 1        
    Eductn . . .  1     
    Cathlc     .     1  
    Infn.M             1
    attr(,"legend")
    [1] 0 ' ' 0.3 '.' 0.6 ',' 0.8 '+' 0.9 '*' 0.95 'B' 1

    cov2cr> symnum(Rp <- cor(swM, method = "kendall", use = "pairwise"))
           F A Ex Ed C I
    Frtlty 1            
    Agrclt   1          
    Exmntn . . 1        
    Eductn . . .  1     
    Cathlc     .     1  
    Infn.M .           1
    attr(,"legend")
    [1] 0 ' ' 0.3 '.' 0.6 ',' 0.8 '+' 0.9 '*' 0.95 'B' 1

    cov2cr> symnum(R. <- cor(swiss, method = "kendall"))
                     F A Ex Ed C I
    Fertility        1            
    Agriculture        1          
    Examination      . . 1        
    Education        . . .  1     
    Catholic             .     1  
    Infant.Mortality .           1
    attr(,"legend")
    [1] 0 ' ' 0.3 '.' 0.6 ',' 0.8 '+' 0.9 '*' 0.95 'B' 1

    cov2cr> ## "pairwise" is closer componentwise,
    cov2cr> summary(abs(c(1 - Rp/R.)))
       Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    0.00000 0.00000 0.04481 0.09573 0.15214 0.53941 

    cov2cr> summary(abs(c(1 - Rc/R.)))
       Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    0.00000 0.02021 0.08482 0.50675 0.16192 7.08509 

    cov2cr> ## but "complete" is closer in Eigen space:
    cov2cr> EV <- function(m) eigen(m, only.values=TRUE)$values

    cov2cr> summary(abs(1 - EV(Rp)/EV(R.)) / abs(1 - EV(Rc)/EV(R.)))
       Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
     0.8942  1.1464  1.2452  1.3732  1.3722  2.3265 

{% endhideToggle %}
## 操作设置

1.  读取工作空间和设置工作目录

``` r
getwd()#读取当前工作目录
setwd(getwd()) # 将当前工作目录设置为当前工作目录、这里是多余的操作只为演示、自己设置、将`getwd()`设置为自己的路径即可
```

‘d:/Statistics/编程相关/R语言/R语言实战’

1.  设置默认展示小数点后两位

<!-- -->

    options(digits = 2)

注意此函数只有当print的时候才有用、如`print(sd(weight))`时才会有用。 \*
options(digits = 2) 影响全局数字显示精度，但不改变函数输出的精度。 \*
使用 round、format 或在 print 中设置 digits 可以控制输出时的显示精度。

``` r
options(digits = 2)
sd(weight) # 输出结果3.80788655293195、并没有保留两位小数  
print(sd(weight)) # 输出结果3.81
```

3.80788655293195

    [1] 3.8

## 矩阵、列表、数据框

### 一般矩阵的创建

    matrix(data = NA, nrow = 1, ncol = 1, byrow = FALSE,
            dimnames = NULL)

- nrow,ncol 是行数和列数
- byrow = FALSE 按列填充 
  matrix都是二维数的、如果你想创建高维的数组、可以通过`array`函数来创建、使用上面提过的、`example(array)`查看相关用法

``` r
demo <- matrix(1:12, ncol = 3)
#更改行名和列名
rownames(demo) <- c("A", "B", "C","D")
colnames(demo) <- c("Jack", "BOb", "Mike")
demo
```

A matrix: 4 × 3 of type int

| <!--/--> | Jack | BOb | Mike |
| -------- | ---- | --- | ---- |
| A        | 1    | 5   | 9    |
| B        | 2    | 6   | 10   |
| C        | 3    | 7   | 11   |
| D        | 4    | 8   | 12   |

使用索引提取其中的元素

``` r
demo[1] #输出1， 提取第一行第一列的元素
demo[1,1] #输出1， 提取第一行第一列的元素
demo[1, ] # 提取第一行元素
demo[, 1] # 提取第一列元素
demo[1:2, c(1, 3)] # 提取1-2行,1和3列的交叉元素
```
![](https://kkdominant.oss-cn-shanghai.aliyuncs.com/img/202406191602496.png)

A matrix: 2 × 2 of type int

| <!--/--> | Jack | Mike |
| -------- | ---- | ---- |
| A        | 1    | 9    |
| B        | 2    | 10   |

### 特殊矩阵

希尔伯特 (Hilbert Matrix)矩阵的创建

> 希尔伯特矩阵是一个以德国数学家大卫·希尔伯特命名的特殊矩阵。它的元素定义为矩阵行列索引之和的倒数，形式为：
>$ H_{ij} = 1/(i+j-1)$

对于一个 $n \times n$ 的希尔伯特矩阵 $H$ ，其第 $i$ 行第 $ j $列的元素可以表示为：

$$
    H = \begin{pmatrix}
1 & \frac{1}{2} & \frac{1}{3} & \cdots & \frac{1}{n} \\
\frac{1}{2} & \frac{1}{3} & \frac{1}{4} & \cdots & \frac{1}{n+1} \\
\frac{1}{3} & \frac{1}{4} & \frac{1}{5} & \cdots & \frac{1}{n+2} \\
\vdots & \vdots & \vdots & \ddots & \vdots \\
\frac{1}{n} & \frac{1}{n+1} & \frac{1}{n+2} & \cdots & \frac{1}{2n-1}
\end{pmatrix}
$$ 

**性质**

- **对称性**: 希尔伯特矩阵是对称矩阵。
- **正定性**: 它是正定矩阵，即对所有非零向量 $x$ ，有 $ x^T H x >0$。
- **条件数**:希尔伯特矩阵的条件数（衡量矩阵是否容易出现数值误差的指标）随着矩阵维数增加而迅速增大，因此其数值稳定性较差。

使用for循环来生成一个4阶希尔伯特矩阵,当然也可以自定义函数来生成一个$n\times n$阶的矩阵、这里就省略了

``` r
Hilbert <- matrix(0,4,4)
for (i in 1:4) {
    for (j in 1:4) {
        Hilbert[i, j] <- 1 / (i + j - 1)
    }
}
Hilbert
```

A matrix: 4 × 4 of type dbl

1.00 \| 0.50 \| 0.33 \| 0.25 \|  
0.50 \| 0.33 \| 0.25 \| 0.20 \|  
0.33 \| 0.25 \| 0.20 \| 0.17 \|  
0.25 \| 0.20 \| 0.17 \| 0.14 \|



### 列表

首先有个疑问什么是列表?和python中的\[ \]一样吗？ 在 R
语言中，列表（list）是一种非常灵活的数据结构，可以容纳不同类型的元素，包括向量、矩阵、数据框、甚至其他列表。这种灵活性使得列表特别适合用于存储异构数据或复杂的数据结构。

``` r
# 创建一个简单的列表
my_list <- list(name = "Alice", age = 25, scores = c(90, 85, 88))
my_list
```

$name  
‘Alice’ 
\$age

25 
\$scores
1.  90
2.  85
3.  88

可以通过使用[ ]访问列表中的单个元素，或使用 $
操作符来访问命名元素：

``` r
# 使用 [[ ]] 访问列表元素
print(my_list[[1]]) # "Alice"
print(my_list[[3]]) # c(90, 85, 88)

# 使用 $ 访问命名元素
print(my_list$name) # "Alice"
print(my_list$scores) # c(90, 85, 88)
```

    [1] "Alice"
    [1] 90 85 88
    [1] "Alice"
    [1] 90 85 88


## 数据框:dataframe

dataframe是R语言中最常用的数据类型之一、它是一种特殊的表格数据结构，用来存储二维数据.

``` r
demo1 <- data.frame(matrix(1:100, ncol = 10));demo1
# 使用head和tail函数查看数据大致结构
head(demo1,3)#查看前面三行、默认6行
tail(demo1,2)#后两行、默认6行
```

A data.frame: 10 × 10


| X1 \<int\> | X2 \<int\> | X3 \<int\> | X4 \<int\> | X5 \<int\> | X6 \<int\> | X7 \<int\> | X8 \<int\> | X9 \<int\> | X10 \<int\> |
| ---------- | ---------- | ---------- | ---------- | ---------- | ---------- | ---------- | ---------- | ---------- | ----------- |
| 1          | 11         | 21         | 31         | 41         | 51         | 61         | 71         | 81         | 91          |
| 2          | 12         | 22         | 32         | 42         | 52         | 62         | 72         | 82         | 92          |
| 3          | 13         | 23         | 33         | 43         | 53         | 63         | 73         | 83         | 93          |
| 4          | 14         | 24         | 34         | 44         | 54         | 64         | 74         | 84         | 94          |
| 5          | 15         | 25         | 35         | 45         | 55         | 65         | 75         | 85         | 95          |
| 6          | 16         | 26         | 36         | 46         | 56         | 66         | 76         | 86         | 96          |
| 7          | 17         | 27         | 37         | 47         | 57         | 67         | 77         | 87         | 97          |
| 8          | 18         | 28         | 38         | 48         | 58         | 68         | 78         | 88         | 98          |
| 9          | 19         | 29         | 39         | 49         | 59         | 69         | 79         | 89         | 99          |
| 10         | 20         | 30         | 40         | 50         | 60         | 70         | 80         | 90         | 100         |

A data.frame: 3 × 10


| <!--/--> | X1 \<int\> | X2 \<int\> | X3 \<int\> | X4 \<int\> | X5 \<int\> | X6 \<int\> | X7 \<int\> | X8 \<int\> | X9 \<int\> | X10 \<int\> |
| -------- | ---------- | ---------- | ---------- | ---------- | ---------- | ---------- | ---------- | ---------- | ---------- | ----------- |
| 1        | 1          | 11         | 21         | 31         | 41         | 51         | 61         | 71         | 81         | 91          |
| 2        | 2          | 12         | 22         | 32         | 42         | 52         | 62         | 72         | 82         | 92          |
| 3        | 3          | 13         | 23         | 33         | 43         | 53         | 63         | 73         | 83         | 93          |

A data.frame: 2 × 10


| <!--/--> | X1 \<int\> | X2 \<int\> | X3 \<int\> | X4 \<int\> | X5 \<int\> | X6 \<int\> | X7 \<int\> | X8 \<int\> | X9 \<int\> | X10 \<int\> |
| -------- | ---------- | ---------- | ---------- | ---------- | ---------- | ---------- | ---------- | ---------- | ---------- | ----------- |
| 9        | 9          | 19         | 29         | 39         | 49         | 59         | 69         | 79         | 89         | 99          |
| 10       | 10         | 20         | 30         | 40         | 50         | 60         | 70         | 80         | 90         | 100         |


## 参考资料

- [Vectors in R - R for Data Science](https://r4ds.had.co.nz/vectors.html)
- [R Documentation on numeric](https://www.rdocumentation.org/packages/base/versions/3.6.2/topics/numeric)
- [Working with Vectors in R - Tutorial Gateway](https://www.tutorialgateway.org/r-vectors/)
