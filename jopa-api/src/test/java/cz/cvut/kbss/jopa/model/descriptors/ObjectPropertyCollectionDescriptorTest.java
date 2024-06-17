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
package cz.cvut.kbss.jopa.model.descriptors;

import cz.cvut.kbss.jopa.model.annotations.OWLObjectProperty;
import cz.cvut.kbss.jopa.model.metamodel.FieldSpecification;
import org.junit.jupiter.api.Test;

import java.net.URI;
import java.util.Collections;
import java.util.Set;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

class ObjectPropertyCollectionDescriptorTest {

    @Test
    void getAttributeContextRetrievesAttributeContextFromElementDescriptor() throws Exception {
        final URI context = URI.create("http://onto.fel.cvut.cz/ontologies/jopa/test");
        final FieldSpecification<?, ?> fs = mock(FieldSpecification.class);
        when(fs.getJavaField()).thenReturn(TestClass.stringAttField());
        final FieldSpecification<?, ?> refFs = mock(FieldSpecification.class);
        when(refFs.getJavaField()).thenReturn(WithReference.class.getDeclaredField("testClass"));
        final ObjectPropertyCollectionDescriptor descriptor = new ObjectPropertyCollectionDescriptor(context, refFs);
        assertEquals(Collections.singleton(context), descriptor.getAttributeContexts(fs));
    }

    @SuppressWarnings("unused")
    private static class WithReference {

        @OWLObjectProperty(iri = "http://onto.fel.cvut.cz/ontologies/jopa/test/reference")
        private Set<TestClass> testClass;
    }
}
