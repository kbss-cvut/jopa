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
package cz.cvut.kbss.ontodriver.rdf4j.connector;

import cz.cvut.kbss.ontodriver.rdf4j.environment.Generator;
import org.eclipse.rdf4j.model.IRI;
import org.eclipse.rdf4j.model.ValueFactory;
import org.eclipse.rdf4j.model.impl.SimpleValueFactory;
import org.junit.jupiter.api.Test;

import java.util.Collections;

import static org.junit.jupiter.api.Assertions.assertEquals;

public class LocalModelTest {

    private final ValueFactory valueFactory = SimpleValueFactory.getInstance();

    private final LocalModel localModel = new LocalModel();

    @Test
    public void containsReturnsTrueWhenStatementWasAddedInLocalModel() {
        final IRI subject = valueFactory.createIRI(Generator.generateUri().toString());
        final IRI property = valueFactory.createIRI(Generator.generateUri().toString());
        localModel.addStatements(Collections
                .singletonList(valueFactory.createStatement(subject, property, valueFactory.createLiteral(117))));
        assertEquals(LocalModel.Contains.TRUE, localModel.contains(subject, property, null, Collections.emptySet()));
    }

    @Test
    public void containsReturnsFalseWhenStatementWasRemovedInLocalModel() {
        final IRI subject = valueFactory.createIRI(Generator.generateUri().toString());
        final IRI property = valueFactory.createIRI(Generator.generateUri().toString());
        localModel.removeStatements(Collections
                .singletonList(valueFactory.createStatement(subject, property, valueFactory.createLiteral(117))));
        assertEquals(LocalModel.Contains.FALSE, localModel.contains(subject, property, null, Collections.emptySet()));
    }

    @Test
    public void containsReturnsUnknownWhenStatementsIsNotInLocalModel() {
        final IRI subject = valueFactory.createIRI(Generator.generateUri().toString());
        final IRI property = valueFactory.createIRI(Generator.generateUri().toString());
        assertEquals(LocalModel.Contains.UNKNOWN, localModel.contains(subject, property, null, Collections.emptySet()));
    }

    @Test
    public void containsReturnsTrueWhenStatementWasAddedInLocalModel_Context() {
        final IRI subject = valueFactory.createIRI(Generator.generateUri().toString());
        final IRI property = valueFactory.createIRI(Generator.generateUri().toString());
        final IRI context = valueFactory.createIRI(Generator.generateUri().toString());
        localModel.addStatements(Collections.singletonList(
                valueFactory.createStatement(subject, property, valueFactory.createLiteral(117), context)));
        assertEquals(LocalModel.Contains.TRUE,
                localModel.contains(subject, property, null, Collections.singleton(context)));
    }

    @Test
    public void containsReturnsFalseWhenStatementWasRemovedInLocalModel_Context() {
        final IRI subject = valueFactory.createIRI(Generator.generateUri().toString());
        final IRI property = valueFactory.createIRI(Generator.generateUri().toString());
        final IRI context = valueFactory.createIRI(Generator.generateUri().toString());
        localModel.removeStatements(Collections.singletonList(
                valueFactory.createStatement(subject, property, valueFactory.createLiteral(117), context)));
        assertEquals(LocalModel.Contains.FALSE,
                localModel.contains(subject, property, null, Collections.singleton(context)));
    }

    @Test
    public void containsReturnsUnknownWhenStatementsIsNotInLocalModel_Context() {
        final IRI subject = valueFactory.createIRI(Generator.generateUri().toString());
        final IRI property = valueFactory.createIRI(Generator.generateUri().toString());
        final IRI context = valueFactory.createIRI(Generator.generateUri().toString());
        assertEquals(LocalModel.Contains.UNKNOWN,
                localModel.contains(subject, property, null, Collections.singleton(context)));
    }
}
