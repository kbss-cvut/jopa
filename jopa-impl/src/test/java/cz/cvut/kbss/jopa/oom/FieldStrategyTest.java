/*
 * JOPA
 * Copyright (C) 2024 Czech Technical University in Prague
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
package cz.cvut.kbss.jopa.oom;

import cz.cvut.kbss.jopa.environment.OWLClassA;
import cz.cvut.kbss.jopa.environment.utils.Generators;
import cz.cvut.kbss.jopa.environment.utils.MetamodelMocks;
import cz.cvut.kbss.jopa.model.annotations.SequenceType;
import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.descriptors.EntityDescriptor;
import cz.cvut.kbss.jopa.model.metamodel.*;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;

import java.net.URI;
import java.time.LocalDate;

import static org.hamcrest.CoreMatchers.instanceOf;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
class FieldStrategyTest {

    @Mock
    private EntityMappingHelper mapperMock;


    private MetamodelMocks metamodelMocks;

    private Descriptor descriptor = new EntityDescriptor();

    @BeforeEach
    void setUp() throws Exception {
        this.metamodelMocks = new MetamodelMocks();
        descriptor = spy(descriptor);
    }

    @Test
    void getAttributeContextRetrievesContextFromEntityDescriptor() {
        final URI context = Generators.createIndividualIdentifier();
        descriptor.addAttributeContext(metamodelMocks.forOwlClassA().stringAttribute(), context);
        final FieldStrategy<?, ?> sut = FieldStrategy.createFieldStrategy(metamodelMocks.forOwlClassA().entityType(),
                metamodelMocks.forOwlClassA().stringAttribute(), descriptor, mapperMock);
        assertEquals(context, sut.getAttributeWriteContext());
        verify(descriptor).getSingleAttributeContext(metamodelMocks.forOwlClassA().stringAttribute());
    }

    @Test
    void createFieldStrategyCreatesPluralDataPropertyStrategyForDataPropertyCollectionAttribute() {
        final EntityType et = mock(EntityType.class);
        final CollectionAttributeImpl att = mock(CollectionAttributeImpl.class);
        when(att.isCollection()).thenReturn(true);
        when(att.getCollectionType()).thenReturn(CollectionType.COLLECTION);
        when(att.isAssociation()).thenReturn(false);
        when(att.getBindableJavaType()).thenReturn(String.class);
        when(att.getElementType()).thenReturn(BasicTypeImpl.get(String.class));
        when(att.getPersistentAttributeType()).thenReturn(Attribute.PersistentAttributeType.DATA);
        final FieldStrategy<?, ?> result = FieldStrategy.createFieldStrategy(et, att, descriptor, mapperMock);
        assertThat(result, instanceOf(PluralDataPropertyStrategy.class));
    }

    @Test
    void createFieldStrategyCreatesSimpleSetStrategyForObjectPropertyCollectionAttribute() {
        final EntityType et = mock(EntityType.class);
        final CollectionAttributeImpl att = mock(CollectionAttributeImpl.class);
        when(att.isCollection()).thenReturn(true);
        when(att.getCollectionType()).thenReturn(CollectionType.COLLECTION);
        when(att.isAssociation()).thenReturn(true);
        when(att.getBindableJavaType()).thenReturn(OWLClassA.class);
        when(att.getElementType()).thenReturn(metamodelMocks.forOwlClassA().entityType());
        when(att.getPersistentAttributeType()).thenReturn(Attribute.PersistentAttributeType.OBJECT);
        final FieldStrategy<?, ?> result = FieldStrategy.createFieldStrategy(et, att, descriptor, mapperMock);
        assertThat(result, instanceOf(SimpleSetPropertyStrategy.class));
    }

    @Test
    void createFieldStrategyCreatesSingularMultilingualStringFieldStrategyForSingularMultilingualStringAttributeMappingDataProperty() {
        final FieldStrategy<?, ?> result = FieldStrategy.createFieldStrategy(metamodelMocks.forOwlClassU().entityType(),
                metamodelMocks.forOwlClassU().uSingularStringAtt(), descriptor, mapperMock);
        assertThat(result, instanceOf(SingularMultilingualStringFieldStrategy.class));
    }

    @Test
    void createFieldStrategyCreatesSingularMultilingualStringFieldStrategyForSingularMultilingualStringAttributeMappingAnnotationProperty() {
        final AbstractAttribute<?, ?> att = metamodelMocks.forOwlClassU().uSingularStringAtt();
        when(att.getPersistentAttributeType()).thenReturn(Attribute.PersistentAttributeType.ANNOTATION);
        final FieldStrategy<?, ?> result = FieldStrategy.createFieldStrategy(metamodelMocks.forOwlClassU().entityType(),
                metamodelMocks.forOwlClassU().uSingularStringAtt(), descriptor, mapperMock);
        assertThat(result, instanceOf(SingularMultilingualStringFieldStrategy.class));
    }

    @Test
    void createFieldStrategyCreatesReferencedListDataPropertyStrategyForDataPropertyReferencedList() {
        final EntityType et = mock(EntityType.class);
        final ListAttributeImpl att = mock(ListAttributeImpl.class);
        when(att.isCollection()).thenReturn(true);
        when(att.getCollectionType()).thenReturn(CollectionType.LIST);
        when(att.getSequenceType()).thenReturn(SequenceType.referenced);
        when(att.getPersistentAttributeType()).thenReturn(Attribute.PersistentAttributeType.DATA);
        when(att.getElementType()).thenReturn(BasicTypeImpl.get(LocalDate.class));

        final FieldStrategy<?, ?> result = FieldStrategy.createFieldStrategy(et, att, descriptor, mapperMock);
        assertThat(result, instanceOf(ReferencedListDataPropertyStrategy.class));
    }
}
