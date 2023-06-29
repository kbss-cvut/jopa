/**
 * Copyright (C) 2023 Czech Technical University in Prague
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
package cz.cvut.kbss.jopa.owlapi.identityreasoner;

import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.reasoner.OWLReasoner;
import org.semanticweb.owlapi.reasoner.OWLReasonerConfiguration;
import org.semanticweb.owlapi.reasoner.OWLReasonerFactory;

import javax.annotation.Nonnull;

@Deprecated
public class OWLAPIIdentityReasonerFactory implements OWLReasonerFactory {

    @Nonnull
    public OWLReasoner createNonBufferingReasoner(@Nonnull OWLOntology ontology) {
        return createNonBufferingReasoner(ontology, null);
    }

    @Nonnull
    public OWLReasoner createNonBufferingReasoner(@Nonnull OWLOntology ontology,
                                                  @Nonnull OWLReasonerConfiguration config) {
        return new OWLAPIIdentityReasoner(ontology);
    }

    @Nonnull
    public OWLReasoner createReasoner(@Nonnull OWLOntology ontology) {
        return createReasoner(ontology, null);
    }

    @Nonnull
    public OWLReasoner createReasoner(@Nonnull OWLOntology ontology,
                                      @Nonnull OWLReasonerConfiguration config) {
        return new OWLAPIIdentityReasoner(ontology);
    }

    @Nonnull
    public String getReasonerName() {
        return "Identity reasoner";
    }
}
