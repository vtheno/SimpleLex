#coding=utf-8
class Lexical:
    class Token:
        pass
    class TokenValue(Token):
        pass
    class Id(TokenValue):
        def __init__(self,id):
            self.id = id # id : string , self.id : string
            # datatype Token = Id of string
        def __repr__(self):
            return "(Id {})".format(repr(self.id))
    class Key(TokenValue):
        def __init__(self,key):
            # key : string
            self.key = key # self.key : string
            # | Key of string 
        def __repr__(self):
            return "(Key {})".format(repr(self.key))
    def scan(self,string):
        # scan : string -> token list
        NotImplementedError
class KeyWord:
    @property
    def alphas(self):
        NotImplementedError
    @property
    def symbols(self):
        NotImplementedError
class functor_Lexical(Lexical):
    def __init__(self,keyword):
        self.KeyWord = keyword
        # Keyword : KeyWord        
        # fLexical : KeyWord -> Lexical
    def isGraph(self,c):
        #t = """! " # $ % & ' ( ) * + , - . / 0 1 2 3 4 5 6 7 8 9 : ; < = > ? @ A B C D E F G H I J K L M N O P Q R S T U V W X Y Z [ \ ] ^ _ ` a b c d e f g h i j k l m n o p q r s t u v w x y z { | } ~""".split()
        t = ['!','"','#','$','%','&','\'','(',')','*','+',',','-','.','/','0','1','2','3','4','5','6','7','8','9',':',';','<','=','>','?','@','A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P','Q','R','S','T','U','V','W','X','Y','Z','[','\\',']','^','_','`','a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p','q','r','s','t','u','v','w','x','y','z','{','|','}','~']
        return c in t
    def isPunct(self,c):
        t = ['!','"','#','$','%', '&',"'",
             '(',')','*','+',',','-','.','/',':',';','<',
             '=','>','?','@','[','\\',']','^','_','`','{','|','}','~']
        return c in t
    def alphaTok(self,a):
        if a in self.KeyWord.alphas():
            return self.Key (a)
        return self.Id (a)
    def symbolic (self,sy,ss):
        lsy = list(sy)
        lss = list(ss)
        #print "symbolic:",repr(sy),repr(ss)
        if lss == []:
            return self.Key(sy),ss
        else:
            c,ss1 = lss[0],lss[1:]
            if sy in self.KeyWord.symbols() or (not self.isPunct(c)):
                #if member (sy,Keyword.symbols) orelse not (Char.isPunct c)
                return self.Key(sy),ss
            else:
                return self.symbolic (sy + c,ss1)
    def dropl(self,fn,ss):
        #right = [ ]
        i = 0
        while i < len(ss) and fn(ss[i]):
            #right .append (ss[i])
            i+=1
        return ss[i:]
    def splitl(self,fn,ss):
        right = []
        i = 0 
        while i < len(ss) and fn(ss[i]):
            right .append (ss[i])
            i+=1
        #print right,i,ss,
        return (right,ss[i:])
    def scaning(self,toks,ss):
        lss = list(ss)
        if lss == []:
            return list(reversed(toks))
        else:
            c,ss1 = lss[0],lss[1:]
            if c.isalnum() :
                id,ss2 = self.splitl (lambda x:x.isalnum(),ss)
                tok = self.alphaTok (''.join(id))
                #print "id,ss2,tok:",id,ss2,repr(tok)
                return self.scaning ([tok] + toks,ss2)
            elif self.isPunct (c):
                tok,ss2 = self.symbolic (str(c),ss1)
                #print "tok,ss2:",tok,repr(ss2)
                return self.scaning ([tok] + toks,ss2)
            else:
                return self.scaning (toks,self.dropl ((lambda c:not self.isGraph(c)),
                                                      ss))
    def scan (self,a):
        return self.scaning ( [ ] ,a)

class SyntaxErrors(Exception): pass
class Parsing:
    def __init__(self,Lex):
        self.Lex = Lex
    def id(self,lst): # right it's equal Parsec succeed or result or return
        if lst == [ ]:
            raise SyntaxErrors ("Identifier excepted")
        x,toks = lst[0],lst[1:]
        if isinstance(x,self.Lex.Id):
            return [(x.id,toks)]
        else:
            raise SyntaxErrors ("Identifier excepted")
    def key(self,a):# right $a 0 like id but it need type Lex.Key
        def ckey(lst):
            #print "key:",lst
            if lst == [ ]:
                raise SyntaxErrors ("Identifier excepted")
            x,toks = lst[0],lst[1:]
            if isinstance(x,self.Lex.Key):
                if a == x.key:
                    return [(a,toks)]
                else:
                    raise SyntaxErrors ("error: " +  str(x))
            else:
                raise SyntaxErrors ("Symbol excepted")
        return ckey
    def empty (self,toks):#right it's Parsec fail or zero
        #return self.mreturn( [ ] )(toks)
        #return ([ ],toks)
        return [ ]
    def Or(self,ph1,ph2):# right || 0 it's Parsec || or ++ or Alt or "Or"
        def curryOr(toks):
            try:
                return ph1(toks)
            except SyntaxErrors , _:
                return ph2(toks)
        return curryOr
    def uncurry(self,f):
        return lambda a,b : f (a)(b)
    def concat(self,lst):
        temp = []
        for i in lst:
            temp.extend(i)
        return temp
    def fmap(self,func,lst):
        temp = []
        while lst!=[]:
            now = lst[0]
            #print now,lst
            try:
                temp.append( func(*now) )
            except Exception,e:
                print e,type(e),e.args
                #temp.append( func(now) )
            finally:
                lst = lst[1:]
        else:
            return temp
    def bind(self,p,f):
        def bind__(inp):
            temp = p (inp)
            #print "bind:",temp
            return self.concat ( self.fmap (self.uncurry(f),temp))
        return bind__
    def mreturn(self,v):
        def curryReturn(inp):
            return [(v,inp)]
        return curryReturn
    def seq(self,p,q):
        return self.bind(p,lambda v : self.bind(q,lambda w : self.mreturn((v,w))))
    def many(self,p):
        return self.Or(
            self.bind(p,lambda x:
            self.bind(self.many(p),lambda xs: self.mreturn ([x]+xs) )),
            self.mreturn([ ]))
    def chainl1(self,p,op):
        def rest(x):
            #print "rest x:",x,type(x)
            return self.Or(
                self.bind(op,lambda f:
                self.bind(p,lambda y :
                          rest(f(x,y))
                          ))
                ,
                self.mreturn(x))
        return self.bind(p,rest)
    def chainr1(self,p,op):
        return self.bind(p,lambda x :
                self.Or(self.bind(op,lambda f:
                        self.bind( self.chainr1(p,op) ,lambda y:
                        self.mreturn ( f (x,y) ))),
                        self.mreturn( x )) )
    def reader(self,ph): # right
        def cR(a):
            tmp = self.Lex.scan(a)
            value = ph (tmp)
            #print type(value),value
            if value == [ ]:
                raise SyntaxErrors ("Extra characters is phrase")
            #print "value:",value
            x,xs = value[0]
            if xs == []:
                return x
            else:
                raise SyntaxErrors ("Extra characters is phrase")
        return cR

def test2():
    # datatype Expr = Var of string
    #               | Lam of string * Expr
    #               | App of Expr * Expr
    class Expr: pass
    class ExprValue(Expr): pass
    class Var(ExprValue):
        def __init__(self,s):
            self.s = s # s:string
        def __repr__(self): 
            return "{}".format(self.s)
    class Lam(ExprValue):
        def __init__(self,argv,body):
            self.argv = argv # argv : string
            self.body = body # body : Expr
        def __repr__(self):
            return "\\ {} -> {}".format(self.argv,self.body)
    class App(ExprValue):
        def __init__(self,t1,t2):
            # t1 : Expr ,t2 : Expr
            self.t1 = t1
            self.t2 = t2
        def __repr__(self):
            return "( {}  {} )".format(self.t1,self.t2)
    class Key(KeyWord): 
        # 定义保留关键字 
        @classmethod
        def alphas(self):
            # 定义保留 词
            return ['it' ]
        @classmethod
        def symbols(self):
            # 定义保留符号
            return ['_',"\\","->","(",")"]
    # var = abc
    # Lam = \ a . t
    # App = (t1 t2)
    mlam = functor_Lexical(Key)
    p = Parsing(mlam)
    def atom(toks):
        return p.id(toks)
    def var(toks):
        return p.bind(atom,lambda x: p.mreturn( Var(x) ))(toks)
    def lam(toks):
        tmp = p.bind( p.key("\\"),
                      lambda _ : p.bind(atom,
                    lambda argv : p.bind(p.key("->"),
                        lambda _ : p.bind(expr,
                        lambda body: p.mreturn( Lam(argv,body) ) ))))
        return tmp(toks)

    def app(toks):
        tmp = p.bind(p.key("("),
                     lambda _ : p.bind(expr,
                    lambda t1 : p.bind (expr,
                    lambda t2 : p.bind (p.key(")"),
                    lambda _ : p.mreturn ( (t1,t2) )))))
                                  
        return tmp(toks)
    def expr(toks):
        #return p.Or(var,p.Or(lam,app))(toks)
        return p.Or(var,p.Or(lam,app))(toks)
    def read(string):
        return p.reader(expr)(string)
    print var(mlam.scan(" abc "))
    print p.reader(var)("abc")
    print lam(mlam.scan(" \\a->b "))
    print p.reader(lam)("\\abc->abc")
    print p.reader(app)(" (a b) ")
    print p.reader(expr)(" (a b) ")
    print p.reader(expr)("\\abc->(abc c)")
    print p.reader(expr)("(abc \\a->a)")
    print p.reader(expr)("(\\a->a b)")
    print p.reader(expr)("(\\a->(a a) b)")
    print p.reader(expr)(" ( (a b) ( \\ a -> a b)) ")
    while 1:
        try:
            inp = raw_input("\n>> ")
            tmp = mlam.scan(inp)
            print tmp
            print "=>",read(inp)
        except Exception,e:
            print e
            continue

def test3():
    # datatype Expr = Let of string * Expr * Expr
    #               | Var of string
    class Expr: pass
    class ExprValue(Expr): pass
    class Var(ExprValue):
        def __init__(self,v):
            self.v = v
        def __repr__(self):
            return "(Var {})".format(self.v)
        def eval(self,env):
            return env.get(self.v)
    class Let(ExprValue):
        def __init__(self,name,t1,t2):
            self.name = name
            self.t1 = t1
            self.t2 = t2
        def __repr__(self):
            return "(Let {} = {} in {})".format(self.name,self.t1,self.t2)
        def eval(self,outEnv):
            env = {}
            tmp = self.t1.eval(outEnv)
            env[self.name] = tmp
            return self.t2.eval(env)
    class keyword(KeyWord):
        @classmethod
        def alphas(self):
            return ["val","let","in"]
        @classmethod
        def symbols(self):
            return ["="]
    key = functor_Lexical(keyword)
    p = Parsing(key)
    def var(toks):
        return p.bind(p.id,lambda v : p.mreturn(Var(v)) )(toks)
    def let(toks):
        return p.bind(p.key("let"),
                      lambda _ : p.bind(p.id,
                    lambda name : p.bind(p.key("="),
                        lambda _ : p.bind(expr,
                        lambda t1 : p.bind(p.key("in"),
                        lambda _  : p.bind(expr,
                        lambda t2 : p.mreturn ( Let (name,t1,t2) )))))))(toks)
    def expr(toks):
        return p.Or ( var,let )(toks)
    def read(inp):
        return p.reader(expr)(inp)
    tmp = read("""let a = b in
                  let c = a in 
                  let d = let e = c in e
                  in d
                  """)
    print tmp
    vara = read("""let a = b in 
                    let c = a in c""")
    print vara
    env = {"b":233}
    print vara.eval(env)
    #print read("let a in b")
    print tmp.eval(env)

def testNumber():
    class Kw(KeyWord):
        @classmethod
        def alphas(self):
            return [ ]
        @classmethod
        def symbols(self):
            return ["+"]
    key = functor_Lexical(Kw)
    p = Parsing(key)
    class Expr: pass
    class Const(Expr):
        def __init__(self,i):
            self.i = i
        def __repr__(self):
            return "{}".format(repr(self.i))#"(Const {})".format(repr(self.i))
        def eval(self):
            return self.i
    class Plus(Expr):
        def __init__(self,e1,e2):
            self.e1 = e1
            self.e2 = e2
        def __repr__(self):
            return "({} + {})".format(self.e1,self.e2)
        def eval(self):
            return self.e1.eval() + self.e2.eval()
    def vv(v):
        #print "vv:",v
        if v.isdigit():
            return p.mreturn(Const(int(v)))
        return p.empty(v)
    def const(toks):
        #print toks
        return p.bind(p.id,vv)(toks)
    def plus(toks):
        return p.bind(const,lambda e1 :
                p.bind(p.key("+"),lambda _ :
                p.bind(const,lambda e2: p.mreturn( Plus (e1,e2) ))))(toks)
    def more_plus(toks):
        return p.many(plus)(toks)
    def adop(v):
        #print "adop:",v
        return p.mreturn( Plus )#lambda a,b:a + b )
    def addop(toks):
        #print "addop:",toks
        return p.bind(p.key("+"),adop)(toks)
    def adds(toks):
        return p.chainl1(const,addop)(toks) # like foldl
        return p.chainr1(const,addop)(toks) # like foldr 
    def read(inp):
        #return p.reader(more_plus)(inp)
        return p.reader(adds)(inp)
    #print read("1+2 3+4")#.eval()
    # print read("1+2+3+4+5") # there need chinal or chainr
    print read("1+2+3+4+5+6+7+8+9+10+11+12+13+14+15")#.eval()
    #print reduce(lambda a,b:(a,b),range(2,16),1) 
    # python reduce is foldl 
    # but in scheme fold is foldl ,reduce is foldr 
if __name__ == '__main__':
    #test1()
    #test2()
    #test3()
    testNumber()
