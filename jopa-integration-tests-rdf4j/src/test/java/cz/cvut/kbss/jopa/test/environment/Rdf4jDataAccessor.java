/*
 * JOPA
 * Copyright (C) 2025 Czech Technical University in Prague
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 3.0 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library.
 */
package cz.cvut.kbss.jopa.test.environment;

import cz.cvut.kbss.jopa.model.EntityManager;
import cz.cvut.kbss.ontodriver.rdf4j.util.Rdf4jUtils;
import org.eclipse.rdf4j.model.IRI;
import org.eclipse.rdf4j.model.Value;
import org.eclipse.rdf4j.model.ValueFactory;
import org.eclipse.rdf4j.model.impl.SimpleValueFactory;
import org.eclipse.rdf4j.repository.Repository;
import org.eclipse.rdf4j.repository.RepositoryConnection;
import org.eclipse.rdf4j.rio.RDFHandler;
import org.eclipse.rdf4j.rio.trig.TriGWriter;

import java.io.StringWriter;
import java.net.URI;
import java.util.Collection;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;


public class Rdf4jDataAccessor implements DataAccessor {

    private final ValueFactory vf = SimpleValueFactory.getInstance();

    @Override
    public void persistTestData(Collection<Quad> data, EntityManager em) {
        final Repository repository = em.unwrap(Repository.class);
        try (final RepositoryConnection connection = repository.getConnection()) {
            connection.begin();
            for (Quad t : data) {
                if (t.getContext() != null) {
                    connection
                            .add(toIri(t.getSubject()), toIri(t.getProperty()), toValue(t.getValue(), t.getLanguage()),
                                    toIri(t.getContext()));
                } else {
                    connection
                            .add(toIri(t.getSubject()), toIri(t.getProperty()), toValue(t.getValue(), t.getLanguage()));
                }

            }
            connection.commit();
        }
    }

    private IRI toIri(URI uri) {
        return vf.createIRI(uri.toString());
    }

    private Value toValue(Object value, String language) {
        return value instanceof URI ? toIri((URI) value) : Rdf4jUtils.createLiteral(value, language, vf);
    }

    @Override
    public void verifyDataPresent(Collection<Quad> data, EntityManager em) {
        final Repository repository = em.unwrap(Repository.class);
        try (final RepositoryConnection connection = repository.getConnection()) {
            data.forEach(t -> assertTrue(doesQuadExist(t, connection)));
        }
    }

    private boolean doesQuadExist(Quad t, RepositoryConnection connection) {
        if (t.getContext() != null) {
            return connection
                    .hasStatement(toIri(t.getSubject()), toIri(t.getProperty()), toValue(t.getValue(), t.getLanguage()),
                            false, toIri(t.getContext()));
        } else {
            return connection
                    .hasStatement(toIri(t.getSubject()), toIri(t.getProperty()), toValue(t.getValue(), t.getLanguage()),
                            false);
        }
    }

    @Override
    public void verifyDataNotPresent(Collection<Quad> data, EntityManager em) {
        final Repository repository = em.unwrap(Repository.class);
        try (final RepositoryConnection connection = repository.getConnection()) {
            data.forEach(t -> assertFalse(doesQuadExist(t, connection)));
        }
    }

    @Override
    public String exportRepository(EntityManager em) {
        final Repository repository = em.unwrap(Repository.class);
        try (final RepositoryConnection connection = repository.getConnection()) {
            final StringWriter strWriter = new StringWriter();
            final RDFHandler writer = new TriGWriter(strWriter);
            connection.export(writer);
            return strWriter.toString();
        }
    }
}
