package week3.Gente

class Director (val nombre: String, val apellido:String, val nacimiento:Int){
  def getnombre : String = s"$nombre $apellido"
  def copy(nombre:String = this.nombre,
           apellido:String = this.apellido,
           nacimiento:Int = this.nacimiento):Director = new Director(nombre,apellido,nacimiento)
}

object Director{
  def apply(nombre:String, apellido:String, nacimiento:Int):Director = new Director(nombre,apellido,nacimiento)
  def esMayor(dir1:Director, dir2:Director):Director = if (dir1.nacimiento > dir1.nacimiento) {
    dir1
  } else {
    dir2
  }
}

class Pelicula(val nombre:String, val presentacion:Int, val rangoIMDB:Double, val director: Director){
  def directorEdad: Int = presentacion-director.nacimiento

  def esDiriginaPor(director: Director): Boolean =this.director == director

  def copy(nombre:String = this.nombre,
           presentacion:Int = this.presentacion,
           rangoIMDB:Double = this.rangoIMDB,
           director: Director= this.director):Pelicula = new Pelicula(nombre,presentacion,rangoIMDB,director)
}

object Pelicula {
  def apply(nombre: String, presentacion: Int, rangoIMDB: Double, director: Director): Pelicula ={
    new Pelicula(nombre, presentacion, rangoIMDB, director)
  }

  def mejorCalificada(pelicula1: Pelicula,pelicula2: Pelicula):Pelicula = if (pelicula1.rangoIMDB > pelicula2.rangoIMDB) {
    pelicula1
  } else {
    pelicula2
  }

  def mayorDirectorEnElTiempo(pelicula1: Pelicula,pelicula2: Pelicula):Pelicula = {
    if (pelicula1.directorEdad > pelicula2.directorEdad) {
      pelicula1
    } else {
      pelicula2
    }
  }
}