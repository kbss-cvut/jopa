package cz.cvut.kbss.jopa.query.sparql.loader;

import cz.cvut.kbss.jopa.environment.OWLClassA;
import cz.cvut.kbss.jopa.query.parameter.ParameterValueFactory;
import cz.cvut.kbss.jopa.query.sparql.Sparql11QueryParser;
import cz.cvut.kbss.jopa.query.sparql.TokenStreamSparqlQueryHolder;
import cz.cvut.kbss.jopa.sessions.UnitOfWork;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.net.URI;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
class SparqlQueryResultLoadingOptimizerTest {

    @Mock
    private UnitOfWork uow;

    private Sparql11QueryParser parser;

    @BeforeEach
    void setUp() {
        this.parser = new Sparql11QueryParser(new ParameterValueFactory(uow));
    }

    @Test
    void optimizeQueryAssemblySetsAssemblyModifierWhenQueryIsSelectAndResultClassIsEntityType() {
        final TokenStreamSparqlQueryHolder qh = spy(parser.parseQuery("SELECT ?s WHERE { ?s a ?type }"));
        final SparqlQueryResultLoadingOptimizer sut = new SparqlQueryResultLoadingOptimizer(qh, uow);
        sut.enableOptimization();
        when(uow.isEntityType(OWLClassA.class)).thenReturn(true);
        sut.optimizeQueryAssembly(OWLClassA.class);
        verify(qh).setAssemblyModifier(any(UnboundPredicateObjectSparqlAssemblyModifier.class));
    }

    @Test
    void optimizeQueryAssemblyDoesNotOptimizeQueryWhenResultClassIsNotEntity() {
        final TokenStreamSparqlQueryHolder qh = spy(parser.parseQuery("SELECT ?s WHERE { ?s a ?type }"));
        final SparqlQueryResultLoadingOptimizer sut = new SparqlQueryResultLoadingOptimizer(qh, uow);
        sut.enableOptimization();
        sut.optimizeQueryAssembly(URI.class);
        verify(qh, never()).setAssemblyModifier(any());
    }

    @Test
    void optimizeQueryAssemblyDoesNotOptimizeQueryWhenLimitIsSet() {
        final TokenStreamSparqlQueryHolder qh = spy(parser.parseQuery("SELECT ?s WHERE { ?s a ?type } LIMIT 1"));
        final SparqlQueryResultLoadingOptimizer sut = new SparqlQueryResultLoadingOptimizer(qh, uow);
        sut.enableOptimization();
        when(uow.isEntityType(OWLClassA.class)).thenReturn(true);
        sut.optimizeQueryAssembly(OWLClassA.class);
        verify(qh, never()).setAssemblyModifier(any());
    }

    @Test
    void optimizeQueryAssemblyDoesNotOptimizeQueryWhenOffsetIsSet() {
        final TokenStreamSparqlQueryHolder qh = spy(parser.parseQuery("SELECT ?s WHERE { ?s a ?type } OFFSET 1"));
        final SparqlQueryResultLoadingOptimizer sut = new SparqlQueryResultLoadingOptimizer(qh, uow);
        sut.enableOptimization();
        when(uow.isEntityType(OWLClassA.class)).thenReturn(true);
        sut.optimizeQueryAssembly(OWLClassA.class);
        verify(qh, never()).setAssemblyModifier(any());
    }

    @Test
    void optimizeQueryAssemblyDoesNotOptimizeQueryWhenItContainsGraphClause() {
        final TokenStreamSparqlQueryHolder qh = spy(parser.parseQuery("SELECT ?s WHERE { GRAPH ?g { ?s a ?type } }"));
        final SparqlQueryResultLoadingOptimizer sut = new SparqlQueryResultLoadingOptimizer(qh, uow);
        sut.enableOptimization();
        when(uow.isEntityType(OWLClassA.class)).thenReturn(true);
        sut.optimizeQueryAssembly(OWLClassA.class);
        verify(qh, never()).setAssemblyModifier(any());
    }
}
