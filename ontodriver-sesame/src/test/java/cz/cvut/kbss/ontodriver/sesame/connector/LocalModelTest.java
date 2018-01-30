package cz.cvut.kbss.ontodriver.sesame.connector;

import cz.cvut.kbss.ontodriver.sesame.environment.Generator;
import org.eclipse.rdf4j.model.IRI;
import org.eclipse.rdf4j.model.ValueFactory;
import org.eclipse.rdf4j.model.impl.SimpleValueFactory;
import org.junit.Test;

import java.util.Collections;

import static org.junit.Assert.assertEquals;

public class LocalModelTest {

    private ValueFactory valueFactory = SimpleValueFactory.getInstance();

    private LocalModel localModel = new LocalModel();

    @Test
    public void containsReturnsTrueWhenStatementWasAddedInLocalModel() {
        final IRI subject = valueFactory.createIRI(Generator.generateUri().toString());
        final IRI property = valueFactory.createIRI(Generator.generateUri().toString());
        localModel.addStatements(Collections
                .singletonList(valueFactory.createStatement(subject, property, valueFactory.createLiteral(117))));
        assertEquals(LocalModel.Contains.TRUE, localModel.contains(subject, property, null, null));
    }

    @Test
    public void containsReturnsFalseWhenStatementWasRemovedInLocalModel() {
        final IRI subject = valueFactory.createIRI(Generator.generateUri().toString());
        final IRI property = valueFactory.createIRI(Generator.generateUri().toString());
        localModel.removeStatements(Collections
                .singletonList(valueFactory.createStatement(subject, property, valueFactory.createLiteral(117))));
        assertEquals(LocalModel.Contains.FALSE, localModel.contains(subject, property, null, null));
    }

    @Test
    public void containsReturnsUnknownWhenStatementsIsNotInLocalModel() {
        final IRI subject = valueFactory.createIRI(Generator.generateUri().toString());
        final IRI property = valueFactory.createIRI(Generator.generateUri().toString());
        assertEquals(LocalModel.Contains.UNKNOWN, localModel.contains(subject, property, null, null));
    }

    @Test
    public void containsReturnsTrueWhenStatementWasAddedInLocalModel_Context() {
        final IRI subject = valueFactory.createIRI(Generator.generateUri().toString());
        final IRI property = valueFactory.createIRI(Generator.generateUri().toString());
        final IRI context = valueFactory.createIRI(Generator.generateUri().toString());
        localModel.addStatements(Collections.singletonList(
                valueFactory.createStatement(subject, property, valueFactory.createLiteral(117), context)));
        assertEquals(LocalModel.Contains.TRUE, localModel.contains(subject, property, null, context));
    }

    @Test
    public void containsReturnsFalseWhenStatementWasRemovedInLocalModel_Context() {
        final IRI subject = valueFactory.createIRI(Generator.generateUri().toString());
        final IRI property = valueFactory.createIRI(Generator.generateUri().toString());
        final IRI context = valueFactory.createIRI(Generator.generateUri().toString());
        localModel.removeStatements(Collections.singletonList(
                valueFactory.createStatement(subject, property, valueFactory.createLiteral(117), context)));
        assertEquals(LocalModel.Contains.FALSE, localModel.contains(subject, property, null, context));
    }

    @Test
    public void containsReturnsUnknownWhenStatementsIsNotInLocalModel_Context() {
        final IRI subject = valueFactory.createIRI(Generator.generateUri().toString());
        final IRI property = valueFactory.createIRI(Generator.generateUri().toString());
        final IRI context = valueFactory.createIRI(Generator.generateUri().toString());
        assertEquals(LocalModel.Contains.UNKNOWN, localModel.contains(subject, property, null, context));
    }
}