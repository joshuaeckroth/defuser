gameCard("{w#-w#=2}", 1).
gameCard("{w#=cn/w#=cn}", 1).
gameCard("{cn/w#=cn/w#}", 1).
gameCard("{bn/gn,yn/rn,rn/bn}", 1).
gameCard("{cn!=cn!=cn!=cn}", 1).
gameCard("{wn^wn^wn^wn^wn}", 1). % i.e., right-most is on top of stack
gameCard("{cn=cn,w#=w#}", 1).
gameCard("{wn>wn<wn}", 1). % i.e., middle number is less than both left and right
gameCard("{cn/w#=cn/w#=cn/w#}", 2).
gameCard("{w6.w5;w4}", 2). % i.e., 6 and 5 form bottom of pyramid, 4 on top
gameCard("{w4>wn>wn}", 2).
gameCard("{w2.w3;w1}", 2).
gameCard("{rn,w2,kn,w6}", 2). % tested
gameCard("{kn.kn;w1}", 2).
gameCard("{wn+wn+wn=11}", 2).
gameCard("{w6,w5,w1}", 2). % tested
gameCard("{wn+wn=wn+wn}", 2).
gameCard("{rn.gn;bn}", 2).
gameCard("{kn/w1,kn/w2,kn/w3}", 2).
gameCard("{bn.yn;bn}", 2).
gameCard("{w1,rn,gn,yn}", 2).
gameCard("{gn.w3;kn}", 2).
gameCard("{bn^kn^rn}", 3).
gameCard("{w3^gn^yn/w1}", 3).
gameCard("{w1^w2^w3}", 3).
gameCard("{w#=w#=w#}", 3). % tested
gameCard("{w5,yn,g2}", 3).
gameCard("{w3,w5,w4,w3}", 3).
gameCard("{cn=cn=cn}", 3).
gameCard("{w2/w4^w1/w3^gn/bn^rn/gn}", 3).
gameCard("{kn/yn^rn/bn^yn/kn^bn/gn}", 3).
gameCard("{rn/kn+bn/w2=7}", 3).
gameCard("{wn+wn+wn=15}", 3).
gameCard("{bn/gn,yn/rn,b4}", 3).
gameCard("{gn/yn+rn/bn=7}", 3).
gameCard("{yn^rn^bn^gn}", 4).
gameCard("{bn^gn^w4^rn/yn}", 4).
gameCard("{w2,w2,w2}", 4).
gameCard("{cn=cn=cn=cn}", 4).
gameCard("{rn^w2^gn^w5}", 4).
gameCard("{gn^kn^rn^bn}", 4).
gameCard("{wn^>wn^>wn^>wn}", 4). % i.e., stacked and numbers get strictly smaller up the stack
gameCard("{wn^<wn^<wn^<wn}", 4). % i.e., stacked and numbers get strictly larger up the stack
gameCard("{kn^kn^yn^yn}", 4).
gameCard("{wn^wn^y1^wn^wn}", 4).
gameCard("{w2/r1,g1/b1,w3/w6}", 4).
gameCard("{w#=w#=b#}", 4).
gameCard("{bn+rn=6}", 4).
gameCard("{c#=c#}", 4).
gameCard("{w1.w3/rn.w2;kn/bn.gn/w4;w5/yn}", 6).
gameCard("{w6.rn.w3;rn.yn;yn}", 6).
gameCard("{kn.rn.kn;bn.gn;yn}", 6).
gameCard("{w1/w4^w2/w5^w3/w6,rn/yn^gn/bn^kn/rn}", 6).
gameCard("{w1/w2^w3/w4^w5^w6,rn/yn^kn/bn^rn/gn}", 6).

