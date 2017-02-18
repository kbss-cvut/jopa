/**
 * Copyright (C) 2016 Czech Technical University in Prague
 * <p>
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any
 * later version.
 * <p>
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.jopa.model.metamodel;

import cz.cvut.kbss.jopa.environment.OWLClassJ;
import cz.cvut.kbss.jopa.exception.MetamodelInitializationException;
import org.junit.Test;
import org.mockito.ArgumentCaptor;

import java.lang.reflect.Field;

import static org.junit.Assert.assertTrue;
import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;

public class ClassFieldMetamodelProcessorTest {

    private MetamodelBuilder metamodelBuilder = new MetamodelBuilder();

    @Test(expected = MetamodelInitializationException.class)
    public void processingNonTransientFieldWithoutPropertyInfoThrowsException() throws Exception {
        final EntityTypeImpl<InvalidClass> etMock = mock(EntityTypeImpl.class);
        final ClassFieldMetamodelProcessor<InvalidClass> processor = new ClassFieldMetamodelProcessor<>(
                InvalidClass.class, etMock, metamodelBuilder);
        final Field field = InvalidClass.class.getDeclaredField("invalidAttribute");
        processor.processField(field);
    }

    private static final class InvalidClass {

        private String invalidAttribute;    // Attribute not transient but has no property/id info

        public String getInvalidAttribute() {
            return invalidAttribute;
        }

        public void setInvalidAttribute(String invalidAttribute) {
            this.invalidAttribute = invalidAttribute;
        }
    }

    @Test
    public void processPluralFieldWithNonEmptyCardinalityConstraintAddsTheConstraintToAttributeSpecification()
            throws Exception {
        final EntityTypeImpl<OWLClassJ> etMock = mock(EntityTypeImpl.class);
        final ClassFieldMetamodelProcessor<OWLClassJ> processor = new ClassFieldMetamodelProcessor<>(OWLClassJ.class,
                etMock, metamodelBuilder);
        final Field field = OWLClassJ.getOwlClassAField();
        processor.processField(field);
        final ArgumentCaptor<Attribute> captor = ArgumentCaptor.forClass(Attribute.class);
        verify(etMock).addDeclaredAttribute(eq(field.getName()), captor.capture());
        assertTrue(captor.getValue().isNonEmpty());
    }
}