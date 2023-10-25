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
package cz.cvut.kbss.ontodriver.owlapi.change;

import org.semanticweb.owlapi.model.OWLDataProperty;
import org.semanticweb.owlapi.model.OWLLiteral;
import org.semanticweb.owlapi.model.OWLNamedIndividual;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.model.OWLOntologyChange;
import org.semanticweb.owlapi.model.RemoveAxiom;
import org.semanticweb.owlapi.search.EntitySearcher;

import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class SubjectDataPropertyRemove extends SubjectPropertyRemove<OWLDataProperty> {

    public SubjectDataPropertyRemove(OWLNamedIndividual subject, OWLDataProperty property) {
        super(subject, property);
    }

    @Override
    public List<OWLOntologyChange> toOwlChanges(OWLOntology targetOntology) {
        final Stream<OWLLiteral> values = EntitySearcher.getDataPropertyValues(subject, property, targetOntology);
        return values.map(value -> new RemoveAxiom(targetOntology, targetOntology.getOWLOntologyManager()
                                                                                 .getOWLDataFactory()
                                                                                 .getOWLDataPropertyAssertionAxiom(property, subject, value)))
                     .collect(Collectors.toList());
    }
}
