/**
 * Copyright (C) 2020 Czech Technical University in Prague
 * <p>
 * This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public
 * License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later
 * version.
 * <p>
 * This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
 * warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License along with this program. If not, see
 * <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.jopa.test.environment;

import cz.cvut.kbss.jopa.model.EntityManager;
import cz.cvut.kbss.ontodriver.jena.connector.StorageConnector;
import cz.cvut.kbss.ontodriver.jena.util.JenaUtils;
import cz.cvut.kbss.ontodriver.model.Assertion;
import cz.cvut.kbss.ontodriver.model.Value;
import org.apache.jena.query.Dataset;
import org.apache.jena.rdf.model.*;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;

import static org.apache.jena.rdf.model.ResourceFactory.*;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

public class JenaDataAccessor implements DataAccessor {

    @Override
    public void persistTestData(Collection<Quad> data, EntityManager em) throws Exception {
        final StorageConnector ds = em.unwrap(StorageConnector.class);
        ds.begin();
        final List<Statement> toAdd = new ArrayList<>(data.size());
        for (Quad t : data) {
            final Resource subject = createResource(t.getSubject().toString());
            final Property property = createProperty(t.getProperty().toString());
            final RDFNode value = toValue(t);
            ds.add(Collections.singletonList(createStatement(subject, property, value)),
                    t.getContext() != null ? t.getContext().toString() : null);
        }
        ds.add(toAdd, null);
        ds.commit();
    }

    private RDFNode toValue(Quad quad) {
        return JenaUtils.valueToRdfNode(Assertion.createPropertyAssertion(quad.getProperty(), quad.getLanguage(), false), new Value<>(quad.getValue()));
    }

    @Override
    public void verifyDataPresent(Collection<Quad> expected, EntityManager em) {
        final Dataset ds = em.unwrap(Dataset.class);
        expected.forEach(t -> assertTrue(doesQuadExist(t,
                t.getContext() == null ? ds.getDefaultModel() : ds.getNamedModel(t.getContext().toString()))));
    }

    private boolean doesQuadExist(Quad quad, Model model) {
        return model.contains(createResource(quad.getSubject().toString()),
                createProperty(quad.getProperty().toString()), toValue(quad));
    }

    @Override
    public void verifyDataNotPresent(Collection<Quad> data, EntityManager em) {
        final Dataset ds = em.unwrap(Dataset.class);
        data.forEach(t -> assertFalse(doesQuadExist(t,
                t.getContext() == null ? ds.getDefaultModel() : ds.getNamedModel(t.getContext().toString()))));
    }
}
