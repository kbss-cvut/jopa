package cz.cvut.kbss.jopa.query.sparql.loader;

import cz.cvut.kbss.jopa.environment.OWLClassA;
import cz.cvut.kbss.jopa.environment.Vocabulary;
import cz.cvut.kbss.jopa.environment.utils.Generators;
import cz.cvut.kbss.jopa.environment.utils.MetamodelMocks;
import cz.cvut.kbss.jopa.exceptions.CardinalityConstraintViolatedException;
import cz.cvut.kbss.jopa.model.MetamodelImpl;
import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.descriptors.EntityDescriptor;
import cz.cvut.kbss.jopa.sessions.UnitOfWork;
import cz.cvut.kbss.jopa.vocabulary.RDF;
import cz.cvut.kbss.ontodriver.exception.OntoDriverException;
import cz.cvut.kbss.ontodriver.iteration.ResultRow;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;

import java.net.URI;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.anyCollection;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
class RowsToAxiomsEntityQueryResultLoaderTest {

    @Mock
    private UnitOfWork uow;

    @Mock
    private MetamodelImpl metamodel;

    private final Descriptor descriptor = new EntityDescriptor();

    @BeforeEach
    void setUp() throws Exception {
        MetamodelMocks metamodelMocks = new MetamodelMocks();
        metamodelMocks.setMocks(metamodel);
        when(uow.getMetamodel()).thenReturn(metamodel);
    }

    @Test
    void loadEntityInstanceReadsInitialRowsIntoAxiomsAndLoadsEntityFromThem() throws OntoDriverException {
        final RowsToAxiomsEntityQueryResultLoader<OWLClassA>
                sut = new RowsToAxiomsEntityQueryResultLoader<>(uow, OWLClassA.class, descriptor);
        final OWLClassA instance = Generators.generateOwlClassAInstance();
        final List<ResultRow> firstResultRows = mockResultRows(instance);
        final ResultRow lastRow = mock(ResultRow.class);
        when(lastRow.getColumnCount()).thenReturn(3);
        when(lastRow.getObject(0, URI.class)).thenReturn(Generators.createIndividualIdentifier());
        when(lastRow.getObject(1, URI.class)).thenReturn(URI.create(RDF.TYPE));
        when(lastRow.getObject(2)).thenReturn(URI.create(Vocabulary.c_OwlClassA));
        final List<ResultRow> rows = Stream.concat(firstResultRows.stream(), Stream.of(lastRow)).toList();
        when(uow.readObjectFromAxioms(eq(OWLClassA.class), anyCollection(), eq(descriptor))).thenReturn(instance);

        final Optional<OWLClassA> result = rows.stream().map(sut::loadResult).filter(Optional::isPresent)
                                               .map(Optional::get).findFirst();
        assertTrue(result.isPresent());
        assertEquals(instance, result.get());
        verify(uow).readObjectFromAxioms(eq(OWLClassA.class), anyCollection(), eq(descriptor));
    }

    private static List<ResultRow> mockResultRows(OWLClassA instance) throws OntoDriverException {
        final ResultRow rowOne = mock(ResultRow.class);
        when(rowOne.getColumnCount()).thenReturn(3);
        when(rowOne.getObject(0, URI.class)).thenReturn(instance.getUri());
        when(rowOne.getObject(1, URI.class)).thenReturn(URI.create(Vocabulary.p_a_stringAttribute));
        when(rowOne.getObject(2)).thenReturn(instance.getStringAttribute());
        final ResultRow rowTwo = mock(ResultRow.class);
        when(rowTwo.getColumnCount()).thenReturn(3);
        when(rowTwo.getObject(0, URI.class)).thenReturn(instance.getUri());
        when(rowTwo.getObject(1, URI.class)).thenReturn(URI.create(RDF.TYPE));
        when(rowTwo.getObject(2)).thenReturn(URI.create(Vocabulary.c_OwlClassA));
        return List.of(rowOne, rowTwo);
    }

    @Test
    void loadResultLoadsMultipleInstancesFromConsecutiveRows() throws OntoDriverException {
        final RowsToAxiomsEntityQueryResultLoader<OWLClassA>
                sut = new RowsToAxiomsEntityQueryResultLoader<>(uow, OWLClassA.class, descriptor);
        final OWLClassA instanceOne = Generators.generateOwlClassAInstance();
        final List<ResultRow> firstResultRows = mockResultRows(instanceOne);
        final OWLClassA instanceTwo = Generators.generateOwlClassAInstance();
        final List<ResultRow> secondResultRows = mockResultRows(instanceTwo);
        final ResultRow lastRow = mock(ResultRow.class);
        when(lastRow.getColumnCount()).thenReturn(3);
        when(lastRow.getObject(0, URI.class)).thenReturn(Generators.createIndividualIdentifier());
        when(lastRow.getObject(1, URI.class)).thenReturn(URI.create(RDF.TYPE));
        when(lastRow.getObject(2)).thenReturn(URI.create(Vocabulary.c_OwlClassA));
        final List<ResultRow> rows = Stream.concat(firstResultRows.stream(), secondResultRows.stream())
                                           .collect(Collectors.toList());
        rows.add(lastRow);
        when(uow.readObjectFromAxioms(eq(OWLClassA.class), anyCollection(), eq(descriptor))).thenReturn(instanceOne)
                                                                                            .thenReturn(instanceTwo);

        final List<OWLClassA> result = rows.stream().map(sut::loadResult).filter(Optional::isPresent)
                                           .map(Optional::get).toList();
        assertEquals(List.of(instanceOne, instanceTwo), result);
        verify(uow, times(2)).readObjectFromAxioms(eq(OWLClassA.class), anyCollection(), eq(descriptor));
    }

    @Test
    void loadLastPendingLoadsEntityInstanceFromPendingAxioms() throws OntoDriverException {
        final RowsToAxiomsEntityQueryResultLoader<OWLClassA>
                sut = new RowsToAxiomsEntityQueryResultLoader<>(uow, OWLClassA.class, descriptor);
        final OWLClassA instance = Generators.generateOwlClassAInstance();
        final List<ResultRow> firstResultRows = mockResultRows(instance);
        when(uow.readObjectFromAxioms(eq(OWLClassA.class), anyCollection(), eq(descriptor))).thenReturn(instance);

        assertTrue(firstResultRows.stream().map(sut::loadResult).noneMatch(Optional::isPresent));
        final Optional<OWLClassA> result = sut.loadLastPending();
        assertTrue(result.isPresent());
        assertEquals(instance, result.get());
        verify(uow).readObjectFromAxioms(eq(OWLClassA.class), anyCollection(), eq(descriptor));
    }

    @Test
    void loadEntityInstanceLoadsEntityUsingNormalUoWLoadWhenLoadFromAxiomsThrowsCardinalityConstraintViolatedException() throws Exception {
        final RowsToAxiomsEntityQueryResultLoader<OWLClassA>
                sut = new RowsToAxiomsEntityQueryResultLoader<>(uow, OWLClassA.class, descriptor);
        final OWLClassA instance = Generators.generateOwlClassAInstance();
        final List<ResultRow> firstResultRows = mockResultRows(instance);
        final ResultRow lastRow = mock(ResultRow.class);
        when(lastRow.getColumnCount()).thenReturn(3);
        when(lastRow.getObject(0, URI.class)).thenReturn(Generators.createIndividualIdentifier());
        when(lastRow.getObject(1, URI.class)).thenReturn(URI.create(RDF.TYPE));
        when(lastRow.getObject(2)).thenReturn(URI.create(Vocabulary.c_OwlClassA));
        final List<ResultRow> rows = Stream.concat(firstResultRows.stream(), Stream.of(lastRow)).toList();
        when(uow.readObjectFromAxioms(eq(OWLClassA.class), anyCollection(), eq(descriptor))).thenThrow(CardinalityConstraintViolatedException.class);
        when(uow.readObject(OWLClassA.class, instance.getUri(), descriptor)).thenReturn(instance);

        final Optional<OWLClassA> result = rows.stream().map(sut::loadResult).filter(Optional::isPresent)
                                               .map(Optional::get).findFirst();
        assertTrue(result.isPresent());
        verify(uow).readObjectFromAxioms(eq(OWLClassA.class), anyCollection(), eq(descriptor));
        verify(uow).readObject(OWLClassA.class, instance.getUri(), descriptor);
    }
}
