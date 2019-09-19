package com.particeep.test

/**
  * This is basic language questions so don't use external library or build in function
  */
object BasicScala {


  /**
    * Encode parameter in url format
    *
    * Example:
    *
    * input  : Map("sort_by" -> "name", "order_by" -> "asc", "user_id" -> "12")
    * output : "?sort_by=name&order_by=asc&user_id=12"
    *
    * input  : Map()
    * output : ""
    */
  def encodeParamsInUrl(params: Map[String, String]): String = {
      var url:String=new String
      if(params.isEmpty) url="" else url="?"
      params foreach (x => url= url+ x._1 + "=" + x._2+"&")
      url.slice(0,url.length-1)
  }


  /**
    * Test if a String is an email
    */
   
  /*je vais utiliser ce regex:
  !eregi("^[_a-z0-9-]+(\.[_a-z0-9-]+)*@[a-z0-9-]+(\.[a-z0-9-]+)*(\.[a-z]{2,3})$
  
  Pour cela je divise le code en 3 partie et je cree une fonction de traitement pour chaque partie
  */
  
  //On traite du debut jusqu'à '@': c'est la partie 1
  def part1IsCorrect(maybeEmail:String):Boolean={
      if(maybeEmail.contains("@")){
        val chars: List[Char] = List('a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p','q','r','s','t','u','v','w','x','y','z','0','1','2','3','4','5','6','7','8','9','_','-')
        var startChar=maybeEmail(0)
        if(!chars.contains(startChar))return false 
        var i=0
        while(maybeEmail(i)!='@'){
          if(!chars.contains(maybeEmail(i)) && maybeEmail(i)!='.')return false
          i += 1
        }
        return true
      }
      return false
  }
  //On traite de '@'  jusqu'au '.': c'est la partie 1
  def part2IsCorrect(maybeEmail:String):Boolean={
      val chars: List[Char] = List('a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p','q','r','s','t','u','v','w','x','y','z','0','1','2','3','4','5','6','7','8','9','-')
      var i:Int=0
      while(maybeEmail(i)!='.'){
          if(!chars.contains(maybeEmail(i)))return false
          i += 1
        }
        return true
  }
  
  //On traite le reste
  def part3IsCorrect(maybeEmail:String):Boolean={
      val chars: List[Char] = List('a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p','q','r','s','t','u','v','w','x','y','z')
      var i:Int=0
      if(maybeEmail.length>=2 && maybeEmail.length<=3){
        for(i<- 0 to maybeEmail.length-1) {
          if(!chars.contains(maybeEmail(i))) return false
        }
        return true
      }
      else return false
  }
  def isEmail(maybeEmail: String): Boolean = {
    var i:Int=0
    var part1:String=new String
    var part2:String=new String
    var part3:String=new String

    if(maybeEmail.contains("@")){
      i=maybeEmail.indexOf("@")
      
      part1=maybeEmail.slice(0, i+1) 
      if(maybeEmail.slice(i+1,maybeEmail.length).contains(".")){
        var reste=maybeEmail.slice(i+1,maybeEmail.length)
        part2=maybeEmail.slice(i+1,reste.indexOf(".")+i+2)
        if(maybeEmail.slice(reste.indexOf(".")+i+3,maybeEmail.length).length>0){
          part3=maybeEmail.slice(reste.indexOf(".")+i+2,maybeEmail.length)
        }
        else return false
      }
      else return false 
    }
    else return false
      
    if (part1IsCorrect(part1)){
      if (part2IsCorrect(part2)){
        if(part3IsCorrect(part3)){
          return true
        }
        else return false
      }
      else return false
    }
    else return false
  }



  /**
    * Compute i ^ n
    *
    * Example:
    *
    * input : (i = 2, n = 3) we compute 2^3 = 2x2x2
    * output : 8
    *
    * input : (i = 99, n = 38997)
    * output : 1723793299
    */
  def power(i:Int, n:Int):Int = {
    var result:Int=1
    if(n==0) return 1
    if(n==1) return i
    for(k<-1 to n){
      result*=i
    }
    return result
  }


}

