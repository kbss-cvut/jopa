/**
 * Copyright (C) 2016 Czech Technical University in Prague
 * <p>
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any
 * later version.
 * <p>
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.jopa.environment;

import cz.cvut.kbss.jopa.model.annotations.*;

import java.lang.reflect.Field;

@OWLClass(iri = Vocabulary.C_OWLClassR)
public class OWLClassR extends OWLClassS {

    @OWLDataProperty(iri = Vocabulary.P_R_STRING_ATTRIBUTE)
    private String stringAtt;

    @OWLObjectProperty(iri = Vocabulary.P_HAS_A, cascade = {CascadeType.PERSIST})
    private OWLClassA owlClassA;

    public String getStringAtt() {
        return stringAtt;
    }

    public void setStringAtt(String stringAtt) {
        this.stringAtt = stringAtt;
    }

    public OWLClassA getOwlClassA() {
        return owlClassA;
    }

    public void setOwlClassA(OWLClassA owlClassA) {
        this.owlClassA = owlClassA;
    }

    @PostLoad
    public void postLoadHook() {
        System.out.println("PostLoad hook called.");
    }

    @Override
    public String toString() {
        return "OWLClassR{" +
                "owlClassA=" + owlClassA +
                ", stringAtt='" + stringAtt + '\'' +
                "} " + super.toString();
    }

    public static String getClassIri() {
        return OWLClassR.class.getDeclaredAnnotation(OWLClass.class).iri();
    }

    public static Field getStringAttField() throws NoSuchFieldException {
        return OWLClassR.class.getDeclaredField("stringAtt");
    }

    public static Field getOwlClassAField() throws NoSuchFieldException {
        return OWLClassR.class.getDeclaredField("owlClassA");
    }
}
