/**
 * Copyright (C) 2023 Czech Technical University in Prague
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
package cz.cvut.kbss.ontodriver.rdf4j.connector;

import cz.cvut.kbss.ontodriver.rdf4j.environment.Generator;
import org.eclipse.rdf4j.model.IRI;
import org.eclipse.rdf4j.model.Statement;
import org.eclipse.rdf4j.model.ValueFactory;
import org.eclipse.rdf4j.model.impl.SimpleValueFactory;
import org.junit.jupiter.api.Test;

import java.util.Collections;
import java.util.List;
import java.util.Set;
import java.util.stream.Stream;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

public class LocalModelTest {

    private final ValueFactory vf = SimpleValueFactory.getInstance();

    private final LocalModel sut = new LocalModel();

    @Test
    public void containsReturnsTrueWhenStatementWasAddedInLocalModel() {
        final IRI subject = vf.createIRI(Generator.generateUri().toString());
        final IRI property = vf.createIRI(Generator.generateUri().toString());
        sut.addStatements(Collections
                .singletonList(vf.createStatement(subject, property, vf.createLiteral(117))));
        assertEquals(LocalModel.Contains.TRUE, sut.contains(subject, property, null, Collections.emptySet()));
    }

    @Test
    public void containsReturnsFalseWhenStatementWasRemovedInLocalModel() {
        final IRI subject = vf.createIRI(Generator.generateUri().toString());
        final IRI property = vf.createIRI(Generator.generateUri().toString());
        sut.removeStatements(Collections
                .singletonList(vf.createStatement(subject, property, vf.createLiteral(117))));
        assertEquals(LocalModel.Contains.FALSE, sut.contains(subject, property, null, Collections.emptySet()));
    }

    @Test
    void containsReturnsFalseWhenStatementSubjectAndPredicateWereRemovedInLocalModel() {
        final IRI subject = vf.createIRI(Generator.generateUri().toString());
        final IRI property = vf.createIRI(Generator.generateUri().toString());
        sut.removeStatementsBySubjectAndPredicate(Set.of(new SubjectPredicateContext(subject, property, Collections.emptySet())));

        assertEquals(LocalModel.Contains.FALSE, sut.contains(subject, property, null, Collections.emptySet()));
    }

    @Test
    public void containsReturnsUnknownWhenStatementsIsNotInLocalModel() {
        final IRI subject = vf.createIRI(Generator.generateUri().toString());
        final IRI property = vf.createIRI(Generator.generateUri().toString());
        assertEquals(LocalModel.Contains.UNKNOWN, sut.contains(subject, property, null, Collections.emptySet()));
    }

    @Test
    public void containsReturnsTrueWhenStatementWasAddedInLocalModel_Context() {
        final IRI subject = vf.createIRI(Generator.generateUri().toString());
        final IRI property = vf.createIRI(Generator.generateUri().toString());
        final IRI context = vf.createIRI(Generator.generateUri().toString());
        sut.addStatements(Collections.singletonList(
                vf.createStatement(subject, property, vf.createLiteral(117), context)));
        assertEquals(LocalModel.Contains.TRUE,
                sut.contains(subject, property, null, Collections.singleton(context)));
    }

    @Test
    public void containsReturnsFalseWhenStatementWasRemovedInLocalModel_Context() {
        final IRI subject = vf.createIRI(Generator.generateUri().toString());
        final IRI property = vf.createIRI(Generator.generateUri().toString());
        final IRI context = vf.createIRI(Generator.generateUri().toString());
        sut.removeStatements(Collections.singletonList(
                vf.createStatement(subject, property, vf.createLiteral(117), context)));
        assertEquals(LocalModel.Contains.FALSE,
                sut.contains(subject, property, null, Collections.singleton(context)));
    }

    @Test
    public void containsReturnsUnknownWhenStatementsIsNotInLocalModel_Context() {
        final IRI subject = vf.createIRI(Generator.generateUri().toString());
        final IRI property = vf.createIRI(Generator.generateUri().toString());
        final IRI context = vf.createIRI(Generator.generateUri().toString());
        assertEquals(LocalModel.Contains.UNKNOWN,
                sut.contains(subject, property, null, Collections.singleton(context)));
    }

    @Test
    void enhanceStatementsRemovesStatementsWhoseSubjectPredicateAndContextMatchRemoved() {
        final IRI subject = vf.createIRI(Generator.generateUri().toString());
        final IRI property = vf.createIRI(Generator.generateUri().toString());
        sut.removeStatementsBySubjectAndPredicate(Set.of(new SubjectPredicateContext(subject, property, Collections.emptySet())));
        final Stream<Statement> stream = Stream.of(
                vf.createStatement(subject, property, vf.createLiteral(1)),
                vf.createStatement(subject, property, vf.createLiteral(2)),
                vf.createStatement(subject, vf.createIRI(Generator.generateUri()
                                                                  .toString()), vf.createLiteral("Three", "en"))
        );

        final List<Statement> result = sut.enhanceStatements(stream, subject, property, null, Collections.emptySet());
        assertTrue(result.stream().noneMatch(s -> s.getSubject().equals(subject) && s.getPredicate().equals(property)));
    }
}
