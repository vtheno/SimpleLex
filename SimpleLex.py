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
        t = """! " # $ % & ' ( ) * + , - . / 0 1 2 3 4 5 6 7 8 9 : ; < = > ? @ A B C D E F G H I J K L M N O P Q R S T U V W X Y Z [ \ ] ^ _ ` a b c d e f g h i j k l m n o p q r s t u v w x y z { | } ~""".split()
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
        if lss == []:
            return self.Key(sy),ss
        else:
            c,ss1 = lss[0],lss[1:]
            if sy in self.KeyWord.symbols() or (not self.isPunct(c)):
                #if member (sy,Keyword.symbols) orelse not (Char.isPunct c)
                return self.Key(sy),ss
            return self.symbolic (sy + c,ss1)#''.join(ss1))
    def dropl(self,fn,ss):
        right = [ ]
        #i = 0
        #while i < len(ss) and not fn(i):
        #for i in ss :
        #    if not fn(i):
        #        result.append (i)
        #return result
        i = 0
        while i < len(ss) and fn(ss[i]):
            right .append (ss[i])
            i+=1
        return ss[i:]
    def splitl(self,fn,ss):
        right = []
        #fail = []
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
                #print id,ss2,tok
                return self.scaning ([tok] + toks,''.join(ss2))
            elif self.isPunct (c):
                tok,ss2 = self.symbolic (str(c),''.join(ss1))
                #print "tok,ss2:",tok,ss2
                return self.scaning ([tok] + toks,''.join(ss2))
            else:
                return self.scaning (toks,self.dropl ((lambda c:not self.isGraph(c)),
                                                      ''.join(ss)))
    def scan (self,a):
        return self.scaning ( [ ] ,a)
#
class LamKey(KeyWord):
    @classmethod
    def alphas(self):
        return []
    @classmethod
    def symbols(self):
        return ["(",")","'","->","'"]
a = functor_Lexical (LamKey)
#print a.alphaTok ("(")
#print a.isPunct ("(")
#print a.scan(" ( 'a -> 'b )")
#print a.Token
class SyntaxErrors(Exception): pass
class Parsing:
    def __init__(self,Lex):
        self.Lex = Lex
    def id(self,lst): # right
        if lst == [ ]:
            raise SyntaxErrors ("Identifier excepted")
        x,toks = lst[0],lst[1:]
        if isinstance(x,self.Lex.Id):
            return (x.id,toks)
        else:
            raise SyntaxErrors ("Identifier excepted")
    def _a(self,a):# right $a 0
        def c_a(lst):
            if lst == [ ]:
                raise SyntaxErrors ("Identifier excepted")
            x,toks = lst[0],lst[1:]
            if isinstance(x,self.Lex.Key):
                if a == x.key:
                    return (a,toks)
                else:
                    raise SyntaxErrors(str(a))
            else:
                raise SyntaxErrors ("Symbol excepted")
        return c_a
    def empty (self,toks):#right
        return ([ ],toks)
    def Or(self,ph1,ph2):# right || 0
        def curryOr(toks):
            try:
                return ph1(toks)
            except SyntaxErrors , _:
                return ph2(toks)
        return curryOr
    def NN(self,ph): # !! 0
        def curryNN(toks):
            try :
                return ph(toks)
            except SyntaxErrors,msg:
                raise Exception ("Syntax error:" + msg)
        return curryNN
    def seq(self,ph1,ph2): # right -- 5
        def currySeq(toks):
            x,toks2 = ph1 (toks)
            y,toks3 = ph2 (toks2)
            return ( (x,y) ,toks3)
        return currySeq
    def bind(self,ph,f): # right >> 3
        def curryBind(toks):
            x,toks2 = ph(toks)
            #print "bind:",x,toks2,f
            r = f(x)
            return (r,toks2)
        return curryBind
    def _seq(self,a,ph): # right $-- 6 
        return self.bind(self.seq(self._a(a),self.NN(ph)),
                         (lambda x:x[1]))
    def repeat(self,ph): #right
        def curryR(toks):
            left = self.bind(self.seq(ph,(self.repeat(ph)))
                             ,(lambda a:[a[0]] + a[1]))
            return self.Or(left,self.empty)(toks)
        return curryR
    def infixes(self,ph,prec_of,Apply):
        def over(k):
            def _o(toks):
                return Next(k)(ph(toks))
            return _o
        def Next(k):
            def _n(inp):
                x,lst = inp
                a,toks = lst[0],lst[1:]
                if isinstance(a,Lex.Key):
                    a = a.key
                    if prec_of(a) < k:
                        return (x,([Lex.Key(a)]+toks))
                    return Next(k)( self.bind(over(prec_of(a)),Apply(a,x)))(toks)
                else:
                    return (x,lst)
            return _n
        return over(0)
    def reader(self,ph): # right
        def cR(a):
            tmp = self.Lex.scan(a)
            value = ph (tmp)
            x,xs = value
            #print "value:",value
            if xs == []:
                return x
            else:
                raise SyntaxErrors ("Extra characters is phrase")
        return cR

class T:
    def read(self,*args,**kw):
        NotImplementedError
class V(T): pass
class Var(V):
    def __init__(self,var):
        self.var = var
    def __repr__(self):
        return "({}:Var)".format(repr(self.var))
class Con(V):
    def __init__(self,string,alst):
        self.string = string
        self.alst = alst
    def __repr__(self):
        return "( {} {s} {} ):Con".format(s=self.string,*self.alst)
p = Parsing(a) # a = functor_Lexical (LamKey)
def makeFun(t1,t2):
    return Con("->",[t1,t2])
def typ(toks):
    # 优先级低的在外 这个类似 Parsec
    #print "typ:",toks
    left =  p.bind( p.seq(atom,p._seq("->",atom)) , lambda x:makeFun(x[0],x[1]) )
    tmp = p.Or(left,atom)
    return tmp(toks)
def atom(toks):
    #print "atom:",toks
    quote =  p.bind( p.seq( p._a("'") ,p.id),lambda x:Var(''.join(x)) )
    # Var o op^ => f o g x = f (g x) => Var (op^ x)
    #bracket = p.bind ( p.seq(p._seq("(",typ),p._a(")")) ,lambda x:x[0])
    bracket = p.bind(p.seq(p._seq("(",typ),p._a(")")),lambda x:x[0])
    return p.Or(quote,bracket)(toks)
def read(toks):
    return p.reader(typ)(toks)
#print (a.scan("'abc"))
#print p.Or(p._a("'"),p.id)(a.scan("abc"))
#print p.repeat(p._a("'"))(a.scan("'''"))
#print p._seq("(",p.id)(a.scan("(abc"))
#print p.bind( p.seq(p.id,p._seq("->",p.id)) , lambda x:makeFun(x[0],x[1]) )(a.scan("abc -> abc"))
#print p.bind(p.seq(p._seq("(",p.id),p._a(")")),lambda x:x[0])(a.scan("(abc)"))
print read(" 'a -> ('b -> 'c) ")
print read(" ('a -> 'b) -> 'c ")
