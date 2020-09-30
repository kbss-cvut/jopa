/**
 * Copyright (C) 2020 Czech Technical University in Prague
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
package cz.cvut.kbss.jopa.sessions.merge;

import cz.cvut.kbss.jopa.environment.OWLClassA;
import cz.cvut.kbss.jopa.environment.OWLClassC;
import cz.cvut.kbss.jopa.environment.OWLClassF;
import cz.cvut.kbss.jopa.environment.Vocabulary;
import cz.cvut.kbss.jopa.environment.utils.Generators;
import cz.cvut.kbss.jopa.environment.utils.MetamodelMocks;
import cz.cvut.kbss.jopa.model.MetamodelImpl;
import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.descriptors.EntityDescriptor;
import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.jopa.model.metamodel.FieldSpecification;
import cz.cvut.kbss.jopa.sessions.UnitOfWorkImpl;
import org.junit.Before;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.Iterator;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

import static org.junit.Assert.*;
import static org.mockito.Matchers.any;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

public class CollectionValueMergerTest {

    @Mock
    private UnitOfWorkImpl uow;

    @Mock
    private MetamodelImpl metamodel;

    private Descriptor descriptor;

    private CollectionValueMerger merger;

    @Before
    public void setUp() throws Exception {
        MockitoAnnotations.initMocks(this);
        final MetamodelMocks mocks = new MetamodelMocks();
        mocks.setMocks(metamodel);
        when(uow.getMetamodel()).thenReturn(metamodel);
        when(uow.isEntityType(any())).thenAnswer(invocation -> {
            final Class<?> arg = (Class<?>) invocation.getArguments()[0];
            try {
                return metamodel.entity(arg) != null;
            } catch (IllegalArgumentException e) {
                return false;
            }
        });

        this.descriptor = new EntityDescriptor();
        this.merger = new CollectionValueMerger(uow, new ManagedTypeValueMerger(uow));
    }

    @Test
    public void mergeMergesUpdatedValuesIntoOriginalSetOfNonManagedTypes() {
        final OWLClassA orig = Generators.generateOwlClassAInstance();
        final OWLClassA merge = new OWLClassA(orig);
        final String addedType = Vocabulary.CLASS_BASE + "AddedType";
        final String removedType = merge.getTypes().iterator().next();
        merge.getTypes().remove(removedType);
        merge.getTypes().add(addedType);
        final EntityType<OWLClassA> et = metamodel.entity(OWLClassA.class);
        final FieldSpecification<? super OWLClassA, ?> typesSpec = et.getTypes();

        merger.mergeValue(typesSpec, orig, orig.getTypes(), merge.getTypes(), descriptor);
        assertTrue(orig.getTypes().contains(addedType));
        assertFalse(orig.getTypes().contains(removedType));
    }

    @Test
    public void mergeReplacesNullCollectionWithMergedOne() {
        final OWLClassA orig = Generators.generateOwlClassAInstance();
        final OWLClassA merge = new OWLClassA(orig);
        orig.setTypes(null);
        final EntityType<OWLClassA> et = metamodel.entity(OWLClassA.class);
        final FieldSpecification<? super OWLClassA, ?> typesSpec = et.getTypes();

        merger.mergeValue(typesSpec, orig, orig.getTypes(), merge.getTypes(), descriptor);
        assertNotNull(orig.getTypes());
        assertTrue(orig.getTypes().equals(merge.getTypes()));
    }

    @Test
    public void mergeReplacesOriginalCollectionWithNullWhenMergedHasNull() {
        final OWLClassA orig = Generators.generateOwlClassAInstance();
        final OWLClassA merge = new OWLClassA(orig);
        merge.setTypes(null);
        final EntityType<OWLClassA> et = metamodel.entity(OWLClassA.class);
        final FieldSpecification<? super OWLClassA, ?> typesSpec = et.getTypes();

        merger.mergeValue(typesSpec, orig, orig.getTypes(), merge.getTypes(), descriptor);
        assertNull(orig.getTypes());
    }

    @Test
    public void mergeLoadsReferencesUsedInMergedCollectionFromStorage() throws Exception {
        final OWLClassF orig = new OWLClassF(Generators.createIndividualIdentifier());
        orig.setSimpleSet(IntStream.range(0, 10).mapToObj(i -> Generators.generateOwlClassAInstance())
                                   .collect(Collectors.toSet()));
        orig.getSimpleSet()
            .forEach(a -> when(uow.readObject(OWLClassA.class, a.getUri(), descriptor)).thenReturn(new OWLClassA(a)));
        final OWLClassF merged = new OWLClassF(orig.getUri());
        merged.setSimpleSet(new HashSet<>());
        final Iterator<OWLClassA> it = orig.getSimpleSet().iterator();
        int i = 0;
        while (it.hasNext()) {
            if (i % 2 == 0) {
                merged.getSimpleSet().add(it.next());
            } else {
                it.next();
            }
            i++;
        }
        for (int j = 0; j < Generators.randomPositiveInt(5); j++) {
            final OWLClassA a = Generators.generateOwlClassAInstance();
            merged.getSimpleSet().add(a);
            when(uow.readObject(OWLClassA.class, a.getUri(), descriptor)).thenReturn(new OWLClassA(a));
        }
        final EntityType<OWLClassF> et = metamodel.entity(OWLClassF.class);
        final FieldSpecification<? super OWLClassF, ?> att = et
                .getFieldSpecification(OWLClassF.getSimpleSetField().getName());

        merger.mergeValue(att, orig, orig.getSimpleSet(), merged.getSimpleSet(), descriptor);
        assertEquals(merged.getSimpleSet().size(), orig.getSimpleSet().size());
        merged.getSimpleSet().forEach(a -> verify(uow).readObject(OWLClassA.class, a.getUri(), descriptor));
    }

    @Test
    public void mergeReplacesOriginalCollectionWithEmptyOneWhenMergedCollectionIsEmpty() throws Exception {
        final OWLClassF orig = new OWLClassF(Generators.createIndividualIdentifier());
        orig.setSimpleSet(IntStream.range(0, 10).mapToObj(i -> Generators.generateOwlClassAInstance())
                                   .collect(Collectors.toSet()));
        final OWLClassF merged = new OWLClassF(orig.getUri());
        merged.setSimpleSet(Collections.emptySet());
        final EntityType<OWLClassF> et = metamodel.entity(OWLClassF.class);
        final FieldSpecification<? super OWLClassF, ?> att = et
                .getFieldSpecification(OWLClassF.getSimpleSetField().getName());

        merger.mergeValue(att, orig, orig.getSimpleSet(), merged.getSimpleSet(), descriptor);
        assertNotNull(orig.getSimpleSet());
        assertTrue(orig.getSimpleSet().isEmpty());
    }

    @Test
    public void mergeLoadsReferencesUsedInMergedListFromStorage() throws Exception {
        final OWLClassC orig = new OWLClassC(Generators.createIndividualIdentifier());
        orig.setReferencedList(IntStream.range(0, 10).mapToObj(i -> Generators.generateOwlClassAInstance())
                                        .collect(Collectors.toList()));
        orig.getReferencedList()
            .forEach(a -> when(uow.readObject(OWLClassA.class, a.getUri(), descriptor)).thenReturn(new OWLClassA(a)));
        final OWLClassC merged = new OWLClassC(orig.getUri());
        merged.setReferencedList(new ArrayList<>());
        for (int i = 0; i < orig.getReferencedList().size(); i++) {
            if (i % 3 == 0) {
                merged.getReferencedList().add(orig.getReferencedList().get(i));
            }
        }
        for (int i = 0; i < Generators.randomPositiveInt(5); i++) {
            final OWLClassA a = Generators.generateOwlClassAInstance();
            merged.getReferencedList().add(a);
            when(uow.readObject(OWLClassA.class, a.getUri(), descriptor)).thenReturn(new OWLClassA(a));
        }
        final EntityType<OWLClassC> et = metamodel.entity(OWLClassC.class);
        final FieldSpecification<? super OWLClassC, ?> att = et
                .getFieldSpecification(OWLClassC.getRefListField().getName());

        merger.mergeValue(att, orig, orig.getReferencedList(), merged.getReferencedList(), descriptor);
        assertEquals(merged.getReferencedList().size(), orig.getReferencedList().size());
        merged.getReferencedList().forEach(a -> verify(uow).readObject(OWLClassA.class, a.getUri(), descriptor));
    }
}
