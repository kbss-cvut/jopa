package cz.cvut.kbss.jopa.query.sparql.loader;

import cz.cvut.kbss.jopa.environment.OWLClassA;
import cz.cvut.kbss.jopa.environment.utils.Generators;
import cz.cvut.kbss.jopa.environment.utils.MetamodelMocks;
import cz.cvut.kbss.jopa.model.MetamodelImpl;
import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.descriptors.EntityDescriptor;
import cz.cvut.kbss.jopa.sessions.UnitOfWork;
import cz.cvut.kbss.ontodriver.exception.OntoDriverException;
import cz.cvut.kbss.ontodriver.iteration.ResultRow;
import cz.cvut.kbss.ontodriver.model.Axiom;
import cz.cvut.kbss.ontodriver.model.Value;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;

import java.net.URI;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Stream;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.anyOf;
import static org.hamcrest.Matchers.emptyCollectionOf;
import static org.hamcrest.Matchers.nullValue;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.anyCollection;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
class AttributeBasedRowsToAxiomsQueryResultLoaderTest {

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
    void loadEntityInstanceReadsRowIntoAxiomsAndLoadsEntityFromThem() throws OntoDriverException {
        final AttributeBasedRowsToAxiomsQueryResultLoader<OWLClassA> sut =
                new AttributeBasedRowsToAxiomsQueryResultLoader<>(uow, OWLClassA.class, descriptor);
        final OWLClassA instance = Generators.generateOwlClassAInstance();
        instance.setTypes(Set.of());
        final List<ResultRow> resultRows = Stream.concat(mockResultRows(instance).stream(), mockResultRows(
                                                         Generators.generateOwlClassAInstance()).stream())
                                                 .toList();
        when(uow.readObjectFromAxioms(eq(OWLClassA.class), anyCollection(), eq(descriptor))).thenReturn(instance);
        final Optional<OWLClassA> result = resultRows.stream().map(sut::loadResult).filter(Optional::isPresent)
                                                     .map(Optional::get).findFirst();
        assertTrue(result.isPresent());
        assertEquals(instance, result.get());
        verify(uow).readObjectFromAxioms(eq(OWLClassA.class), anyCollection(), eq(descriptor));
    }

    private static List<ResultRow> mockResultRows(OWLClassA instance) throws OntoDriverException {
        final List<String> allTypes = Stream.concat(Stream.of(OWLClassA.getClassIri()), instance.getTypes().stream())
                                            .toList();
        final List<ResultRow> rows = new ArrayList<>(allTypes.size());
        for (String type : allTypes) {
            final ResultRow row = mock(ResultRow.class);
            when(row.getColumnCount()).thenReturn(3);
            when(row.getColumnNames()).thenReturn(List.of("x", "xstringAttribute", "xtypes"));
            when(row.getObject(0, URI.class)).thenReturn(instance.getUri());
            if (instance.getStringAttribute() != null) {
                when(row.isBound("xstringAttribute")).thenReturn(true);
                when(row.getObject("xstringAttribute")).thenReturn(instance.getStringAttribute());
            }
            when(row.isBound("xtypes")).thenReturn(true);
            when(row.getObject("xtypes")).thenReturn(URI.create(type));
            rows.add(row);
        }
        return rows;
    }

    @Test
    void loadLastPendingLoadsLastPendingEntity() throws Exception {
        final AttributeBasedRowsToAxiomsQueryResultLoader<OWLClassA> sut =
                new AttributeBasedRowsToAxiomsQueryResultLoader<>(uow, OWLClassA.class, descriptor);
        final OWLClassA instance = Generators.generateOwlClassAInstance();
        instance.setTypes(Set.of());
        final List<ResultRow> resultRows = mockResultRows(instance);
        when(uow.readObjectFromAxioms(eq(OWLClassA.class), anyCollection(), eq(descriptor))).thenReturn(instance);
        resultRows.forEach(row -> {
            final Optional<OWLClassA> opt = sut.loadResult(row);
            assertFalse(opt.isPresent());
        });
        final Optional<OWLClassA> result = sut.loadLastPending();
        assertTrue(result.isPresent());
        assertEquals(instance, result.get());
        verify(uow).readObjectFromAxioms(eq(OWLClassA.class), anyCollection(), eq(descriptor));
    }

    @Test
    void loadResultDoesNotDuplicateAlreadyPresentAttributes() throws Exception {
        final AttributeBasedRowsToAxiomsQueryResultLoader<OWLClassA> sut =
                new AttributeBasedRowsToAxiomsQueryResultLoader<>(uow, OWLClassA.class, descriptor);
        final OWLClassA instance = Generators.generateOwlClassAInstance();
        instance.setTypes(Set.of());
        final List<ResultRow> resultRows = mockResultRows(instance);
        when(uow.readObjectFromAxioms(eq(OWLClassA.class), anyCollection(), eq(descriptor))).thenReturn(instance);
        resultRows.forEach(row -> {
            final Optional<OWLClassA> opt = sut.loadResult(row);
            assertFalse(opt.isPresent());
        });
        final Optional<OWLClassA> result = sut.loadLastPending();
        assertTrue(result.isPresent());
        final ArgumentCaptor<Collection<Axiom<?>>> captor = ArgumentCaptor.forClass(Collection.class);
        verify(uow).readObjectFromAxioms(eq(OWLClassA.class), captor.capture(), eq(descriptor));
        assertEquals(1, captor.getValue().stream()
                              .filter(ax -> ax.getValue().equals(new Value<>(instance.getStringAttribute()))).count());
    }

    @Test
    void loadResultHandlesUnboundAttributeVariables() throws Exception {
        final AttributeBasedRowsToAxiomsQueryResultLoader<OWLClassA> sut =
                new AttributeBasedRowsToAxiomsQueryResultLoader<>(uow, OWLClassA.class, descriptor);
        final OWLClassA instance = Generators.generateOwlClassAInstance();
        instance.setTypes(Set.of());
        instance.setStringAttribute(null);
        final List<ResultRow> resultRows = mockResultRows(instance);
        when(uow.readObjectFromAxioms(eq(OWLClassA.class), anyCollection(), eq(descriptor))).thenReturn(instance);
        resultRows.forEach(row -> {
            final Optional<OWLClassA> opt = sut.loadResult(row);
            assertFalse(opt.isPresent());
        });
        final Optional<OWLClassA> result = sut.loadLastPending();
        assertTrue(result.isPresent());
        assertNull(result.get().getStringAttribute());
        assertThat(result.get().getTypes(), anyOf(nullValue(), emptyCollectionOf(String.class)));
    }
}
