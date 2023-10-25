/*
 * Copyright (C) 2023 Czech Technical University in Prague
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any
 * later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.jopa.test;

import cz.cvut.kbss.jopa.model.annotations.*;

@SparqlResultSetMapping(name = OWLClassT.MAPPING_NAME, entities = {
        @EntityResult(entityClass = OWLClassT.class, fields = {
                @FieldResult(name = "uri", variable = "x"),
                @FieldResult(name = "name", variable = "label"),
                @FieldResult(name = "owlClassA", variable = "y")
        })
})
@OWLClass(iri = Vocabulary.C_OWL_CLASS_T)
public class OWLClassT extends OWLClassS {

    public static final String MAPPING_NAME = "OWLClassT.mapping";

    @OWLDataProperty(iri = Vocabulary.P_T_INTEGER_ATTRIBUTE)
    private Integer intAttribute;

    @OWLObjectProperty(iri = Vocabulary.P_HAS_OWL_CLASS_A)
    private OWLClassA owlClassA;

    public Integer getIntAttribute() {
        return intAttribute;
    }

    public void setIntAttribute(Integer intAttribute) {
        this.intAttribute = intAttribute;
    }

    public OWLClassA getOwlClassA() {
        return owlClassA;
    }

    public void setOwlClassA(OWLClassA owlClassA) {
        this.owlClassA = owlClassA;
    }

}
