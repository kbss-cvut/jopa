/**
 * Copyright (C) 2019 Czech Technical University in Prague
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
package cz.cvut.kbss.jopa.test.environment;

import cz.cvut.kbss.jopa.model.EntityManager;
import cz.cvut.kbss.ontodriver.jena.connector.StorageConnector;
import org.apache.jena.query.Dataset;
import org.apache.jena.rdf.model.*;

import java.net.URI;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import static org.apache.jena.rdf.model.ResourceFactory.*;
import static org.junit.Assert.assertTrue;

public class JenaDataAccessor implements DataAccessor {

    @Override
    public void persistTestData(Collection<Triple> data, EntityManager em) throws Exception {
        final StorageConnector ds = em.unwrap(StorageConnector.class);
        ds.begin();
        final List<Statement> toAdd = new ArrayList<>(data.size());
        for (Triple t : data) {
            final Resource subject = createResource(t.getSubject().toString());
            final Property property = createProperty(t.getProperty().toString());
            final RDFNode value;
            if (t.getValue() instanceof URI) {
                value = createResource(t.getValue().toString());
            } else {
                value = t.getValue() instanceof String ?
                        ResourceFactory.createLangLiteral(t.getValue().toString(), t.getLanguage()) :
                        ResourceFactory.createTypedLiteral(t.getValue());
            }
            toAdd.add(createStatement(subject, property, value));
        }
        ds.add(toAdd, null);
        ds.commit();
    }

    @Override
    public void verifyDataPresence(Collection<Triple> expected, EntityManager em) {
        final Dataset ds = em.unwrap(Dataset.class);
        final Model model = ds.getDefaultModel();
        for (Triple t : expected) {
            if (t.getValue() instanceof URI) {
                assertTrue(model.contains(createResource(t.getSubject().toString()),
                        createProperty(t.getProperty().toString()), createResource(t.getValue().toString())));
            } else {
                if (t.getLanguage() != null) {
                    assertTrue(model.contains(createResource(t.getSubject().toString()),
                            createProperty(t.getProperty().toString()),
                            createLangLiteral(t.getValue().toString(), t.getLanguage())));
                } else {
                    assertTrue(model.contains(createResource(t.getSubject().toString()),
                            createProperty(t.getProperty().toString()), createTypedLiteral(t.getValue())));
                }
            }
        }
    }
}
