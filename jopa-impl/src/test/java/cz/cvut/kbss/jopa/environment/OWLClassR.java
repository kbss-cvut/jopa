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
import java.lang.reflect.Method;

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

    @PrePersist
    public void prePersist() {
        System.out.println("PrePersist called.");
    }

    @PostPersist
    public void postPersist() {
        System.out.println("PostPersist called.");
    }

    @PreUpdate
    public void preUpdate() {
        System.out.println("PreUpdate called.");
    }

    @PostUpdate
    public void postUpdate() {
        System.out.println("PostUpdate called.");
    }

    @PreRemove
    public void preRemove() {
        System.out.println("PreRemove called.");
    }

    @PostRemove
    public void postRemove() {
        System.out.println("PostRemove called.");
    }

    @PostLoad
    public void postLoad() {
        System.out.println("PostLoad called.");
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

    public static Method getPrePersistHook() throws Exception {
        return OWLClassR.class.getDeclaredMethod("prePersist");
    }

    public static Method getPostPersistHook() throws Exception {
        return OWLClassR.class.getDeclaredMethod("postPersist");
    }

    public static Method getPreUpdateHook() throws Exception {
        return OWLClassR.class.getDeclaredMethod("preUpdate");
    }

    public static Method getPostUpdateHook() throws Exception {
        return OWLClassR.class.getDeclaredMethod("postUpdate");
    }

    public static Method getPreRemoveHook() throws Exception {
        return OWLClassR.class.getDeclaredMethod("preRemove");
    }

    public static Method getPostRemoveHook() throws Exception {
        return OWLClassR.class.getDeclaredMethod("postRemove");
    }

    public static Method getPostLoadHook() throws Exception {
        return OWLClassR.class.getDeclaredMethod("postLoad");
    }
}
