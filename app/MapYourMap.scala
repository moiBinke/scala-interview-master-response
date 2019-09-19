package com.particeep.test

import scala.collection.mutable.ListBuffer

/**
  * Tell developer names by the department code
  * Expected result:
  * Map(frontend -> List(Remy, Alexandre), analytics -> List(Pierre), api -> List(Noe))
  */
object MapYourMap {

  val devNames = Map("dev1" -> "Pierre", "dev2" -> "Remy", "dev3" -> "Noe", "dev4" -> "Alexandre")
  val devDepartments = Map("dev1" -> "analytics", "dev2" -> "frontend", "dev3" -> "api", "dev4" -> "frontend")



  //Cette méthode renvoie le Map (devX->departementDonné)  d'un departement donnné:tout devX de ce departement
  def listeDevOfThisDepart(department:String,departmentNames:Map[String,String]):Map[String,String]={
    var liste:List[String]= List()
    return departmentNames.filter((t) => t._2 == department)
  }
  
  //Cette méthode renvoie la liste de noms des developpeurs se trouvant dans le map retourné par la méthode listeDevOfThisDepart:Liste de nom de tous les developpeurs de ce departement 
  def listeNamesOfTheseDev(mapOfDevDep:Map[String,String],devNames:Map[String,String]):List[String]={
    var liste = new ListBuffer[String]()
    for((k,v)<-mapOfDevDep){
      liste+=devNames(k)
    }
    return liste.toList
  }
  //Notre fonctio principale
  def nameInDepartmentFonction(devNames:Map[String,String],devDepartments:Map[String,String]):Map[String,List[String]]={
   var result:Map[String,List[String]]=Map()
    for((k,v)<-devDepartments){
     var liste:List[String]= listeNamesOfTheseDev(listeDevOfThisDepart(v,devDepartments),devNames)
      result+=(v->liste)
    }
    return result
  }

  val namesInDepartments:Map[String, List[String]] = nameInDepartmentFonction(devNames,devDepartments)
  
}
