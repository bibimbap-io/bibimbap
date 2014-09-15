package io.bibimbap.tests
package bibtex

import io.bibimbap.bibtex._

import scala.io.Source

import org.scalatest.FunSuite
import org.scalatest.Matchers

class BibTeXNames extends FunSuite with Matchers {
  // In these tests we use "" instead of None... Just simpler.
  def pam(src : String, first : String, von : String, last : String, jr : String) {
    def lift(str : String) : Option[String] = if(str.isEmpty) None else Some(str)

    val NameParts(f,v,l,j) = NameParts.fromString(src)

    lift(first) should equal (f)
    lift(von)   should equal (v)
    lift(last)  should equal (l)
    lift(jr)    should equal (j) 
  }

  test("Name entries w/o commas") {
    pam("Mike Doe", "Mike", "", "Doe", "")
    pam("John von Neumann", "John", "von", "Neumann", "")
    pam("Ludwig  Freiherr von und  zu der Tann-Rathsamhausen", "Ludwig Freiherr", "von und zu der", "Tann-Rathsamhausen", "")
    pam("Rambo", "", "", "Rambo", "")
    pam("bibimbap", "", "", "bibimbap", "")

    pam("jean de la fontaine", "", "jean de la", "fontaine", "")
    pam("Jean de la fontaine", "Jean", "de la", "fontaine", "")
    pam("Jean De La Fontaine", "Jean De La", "", "Fontaine", "")
    pam("jean De la Fontaine", "", "jean De la", "Fontaine", "")
    pam("jean De la fontaine", "", "jean De la", "fontaine", "")
    pam("Jean de La Fontaine", "Jean", "de", "La Fontaine", "")
  }

  test("Name entries w/ one comma") {
    pam("Doe, Mike", "Mike", "", "Doe", "")
    pam("von Neumann, John", "John", "von", "Neumann", "")
    pam("von und  zu der Tann-Rathsamhausen, Ludwig Freiherr", "Ludwig Freiherr", "von und zu der", "Tann-Rathsamhausen", "")

    // This is technically a mistake, but we like to handle it anyway
    pam("jean de la fontaine,", "", "jean de la", "fontaine", "")
    pam("de la fontaine, Jean", "Jean", "de la", "fontaine", "")
    pam("De La Fontaine, Jean", "Jean", "", "De La Fontaine", "")
    pam("De la Fontaine, Jean", "Jean", "De la", "Fontaine", "")
    pam("de La Fontaine, Jean", "Jean", "de", "La Fontaine", "")
  }

  test("Name entries w/ two commas") {
    pam("Gates, III, William Henry", "William Henry", "", "Gates", "III")
    pam("Bush, Jr., George W.", "George W.", "", "Bush", "Jr.")
  }
}
