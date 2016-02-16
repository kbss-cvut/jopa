/**
 * Copyright (C) 2011 Czech Technical University in Prague
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
package cz.cvut.kbss.ontodriver.sesame;

import cz.cvut.kbss.ontodriver.sesame.connector.Connector;
import cz.cvut.kbss.ontodriver.sesame.exceptions.SesameDriverException;
import cz.cvut.kbss.ontodriver.descriptor.AxiomDescriptor;
import cz.cvut.kbss.ontodriver.model.Assertion;
import cz.cvut.kbss.ontodriver.model.NamedResource;
import cz.cvut.kbss.ontodriver.model.Value;
import org.openrdf.model.Resource;
import org.openrdf.model.Statement;
import org.openrdf.model.URI;
import org.openrdf.model.ValueFactory;

import java.util.*;

/**
 * This class performs an epistemic remove of axioms described by the axiom
 * descriptor. </p>
 * <p/>
 * Epistemic remove means that only information known to the application is
 * deleted. The assertions in the descriptor represent this information. Thus,
 * only these assertions are removed from the ontology. Note that if the
 * descriptor contains an unspecified property assertion, all property
 * assertions related to the subject individual are removed from the property's
 * context.
 *
 * @author ledvima1
 */
class EpistemicAxiomRemover {

    private final Connector connector;
    private final ValueFactory valueFactory;
    private final String language;

    EpistemicAxiomRemover(Connector connector, ValueFactory valueFactory, String language) {
        this.connector = connector;
        this.valueFactory = valueFactory;
        this.language = language;
    }

    void remove(AxiomDescriptor axiomDescriptor) throws SesameDriverException {
        final Resource individual = SesameUtils.toSesameUri(axiomDescriptor.getSubject()
                .getIdentifier(), valueFactory);
        final Collection<Statement> toRemove = new HashSet<>();
        for (Assertion a : axiomDescriptor.getAssertions()) {
            if (a.isInferred()) {
                continue;
            }
            toRemove.addAll(connector.findStatements(individual,
                    SesameUtils.toSesameUri(a.getIdentifier(), valueFactory), null, a.isInferred(),
                    SesameUtils.toSesameUri(axiomDescriptor.getAssertionContext(a), valueFactory)));
        }
        connector.removeStatements(toRemove);
    }

    void remove(NamedResource individual, Map<Assertion, Set<Value<?>>> values, java.net.URI context) throws SesameDriverException {
        final URI sesameContext = SesameUtils.toSesameUri(context, valueFactory);
        final Resource subject = SesameUtils.toSesameUri(individual.getIdentifier(), valueFactory);
        final Collection<Statement> toRemove = new ArrayList<>();
        final SesameValueConverter valueConverter = new SesameValueConverter(valueFactory, language);
        for (Map.Entry<Assertion, Set<Value<?>>> entry : values.entrySet()) {
            final URI property = SesameUtils.toSesameUri(entry.getKey().getIdentifier(), valueFactory);
            for (Value<?> val : entry.getValue()) {
                final org.openrdf.model.Value sesameValue = valueConverter.toSesameValue(entry.getKey(), val);
                if (sesameContext != null) {
                    toRemove.add(valueFactory.createStatement(subject, property, sesameValue, sesameContext));
                } else {
                    toRemove.add(valueFactory.createStatement(subject, property, sesameValue));
                }
            }
        }
        connector.removeStatements(toRemove);
    }
}
