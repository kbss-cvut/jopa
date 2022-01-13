/**
 * Copyright (C) 2022 Czech Technical University in Prague
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any
 * later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.jopa.test;

import cz.cvut.kbss.jopa.model.annotations.OWLClass;
import cz.cvut.kbss.jopa.model.annotations.OWLObjectProperty;

@OWLClass(iri = Vocabulary.C_OWL_CLASS_U)
public class OWLClassU extends OWLClassS {

    // Polymorphic attribute
    @OWLObjectProperty(iri = Vocabulary.P_HAS_OWL_CLASS_S)
    private OWLClassS owlClassS;

    public OWLClassS getOwlClassS() {
        return owlClassS;
    }

    public void setOwlClassS(OWLClassS owlClassS) {
        this.owlClassS = owlClassS;
    }
}
