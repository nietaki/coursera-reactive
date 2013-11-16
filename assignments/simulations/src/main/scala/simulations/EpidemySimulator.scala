package simulations

import math.random
import scala.collection.mutable.Map
import scala.collection.immutable.Set

class EpidemySimulator extends Simulator {

  def randomBelow(i: Int) = (random * i).toInt
  def happens(d: Double):Boolean = random <= d
  
  def getNeigbors(pos: Coords): List[Coords] = {
    List( neighbor(pos, (-1, 0)),
          neighbor(pos,(1, 0)),
          neighbor(pos,(0, -1)),
          neighbor(pos,(0, 1))
        )
  }
  
  def neighbor(pos: Coords, dir: Coords): Coords = {
    //the additional roomCols to be able to subtract small numbers
    Tuple2((pos._1 + dir._1 + SimConfig.roomRows) % SimConfig.roomRows, (pos._1 + dir._1 + SimConfig.roomColumns) % SimConfig.roomColumns)
  }
  
  
  protected[simulations] object SimConfig {
    val population: Int = 300
    val roomRows: Int = 8
    val roomColumns: Int = 8
    
    // to complete: additional parameters of simulation
    val incubationTime: Int = 6
    val probableDeathTime: Int = 14
    val deathProbability: Double = 0.25
    val immunityTime: Int = 16
    val healthTime: Int = 18
    
    val transmitablility: Double = 0.4
    val prevalence: Double = 0.1
  }

  import SimConfig._

  val persons: List[Person] = (0 to population)
      .map(id => new Person(id))
      .toList
      
  persons.foreach(p => if(happens(prevalence)) infect(p))
  
  val flat = new Flat
  
  def moveRandomly(p: Person): Unit = {
    if(!p.dead) {
      val n = getNeigbors(p.coords)
      //potential rooms to go to
      val pot = n.filter(p => flat.getSet(p).forall(! _.sick)) //nice!
      if(!pot.isEmpty) {
        val target: Coords = pot(randomBelow(pot.length))
        flat.movePerson(p, target)
      }
      //TODO plan next random move
    }
  }
  //actual infection
  def infect(p: Person): Unit = {
    //you have to be really healthy to get sick
    if(!p.infected && !p.sick && !p.immune && !p.dead) {
      p.infected = true
    }
  }
  
  def probablyDie(p: Person): Unit = {
    if(happens(deathProbability)) {
      p.dead = true
    }
  }
  
  def getSick(p: Person): Unit = {
    if(!p.dead) {
      p.sick = true
    }
  }
  
  def immunize(p: Person): Unit = {
    if(!p.dead) {
      p.sick = false
      p.immune = true
    }
  }
  
  def recover(p: Person): Unit = {
    if(!p.dead) {
      p.immune = false
      p.infected = false
    }
  }
  
  class Person (val id: Int) {
    var infected = false
    var sick = false
    var immune = false
    var dead = false
    
    //private var actions: List[Simulator#Action] = List()

    // demonstrates random number generation
    var row: Int = randomBelow(roomRows)
    var col: Int = randomBelow(roomColumns)
    def coords: Coords = (row, col)
    //
    // to complete with simulation logic
    //
    flat.insertPerson((row, col), this)
  }
  type Coords = Tuple2[Int, Int]
  class Flat {
    val rooms: Map[Coords, Set[Person]] = Map()
    
    def getSet(pos: Coords): Set[Person] = {
      rooms.getOrElse(pos, Set())
    }
    
    def insertPerson(pos: Coords, p: Person) = {
      rooms += Tuple2(pos, getSet(pos) + p)
    }
    
    def removePerson(pos: Coords, p: Person) = {
      rooms += Tuple2(pos, getSet(pos) - p)
    } 
   
    def movePerson(p:Person, to: Coords) = {
      val from: Coords = (p.row, p.col)
      removePerson(from, p)
      p.row = to._1
      p.col = to._2
      insertPerson(to, p)
    }
  }
}
