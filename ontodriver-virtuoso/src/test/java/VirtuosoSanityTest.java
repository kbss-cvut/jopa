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
import org.eclipse.rdf4j.model.IRI;
import org.eclipse.rdf4j.model.Statement;
import org.eclipse.rdf4j.model.ValueFactory;
import org.eclipse.rdf4j.model.util.Values;
import org.eclipse.rdf4j.model.vocabulary.XSD;
import org.eclipse.rdf4j.repository.Repository;
import org.eclipse.rdf4j.repository.RepositoryConnection;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;
import virtuoso.rdf4j.driver.VirtuosoRepository;

import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;

public class VirtuosoSanityTest {

    @Disabled
    @Test
    void testIntDatatypeWriteRead() {
        Repository repository = new VirtuosoRepository("jdbc:virtuoso://localhost:1111/log_enable=1", "dba", "fj7Njqj2");
        try {
            final IRI subj = Values.iri("http://example.com/subject");
            final IRI property = Values.iri("http://example.com/property");
            final ValueFactory vf = repository.getValueFactory();
            try (final RepositoryConnection conn = repository.getConnection()) {
                conn.begin();
                final List<Statement> statements = List.of(vf.createStatement(subj, property, vf.createLiteral("1", XSD.INT)),
                        vf.createStatement(subj, property, vf.createLiteral("true", XSD.BOOLEAN))
                );
                conn.add(statements);
                conn.commit();
            }

            try (final RepositoryConnection conn = repository.getConnection()) {
                final List<Statement> result = conn.getStatements(subj, null, null, false).stream().toList();
                assertEquals(2, result.size());
            }
        } finally {
            repository.shutDown();
        }
    }
}
