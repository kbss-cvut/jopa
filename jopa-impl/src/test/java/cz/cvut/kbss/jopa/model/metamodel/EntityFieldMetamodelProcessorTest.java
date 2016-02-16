/**
 * Copyright (C) 2011 Czech Technical University in Prague
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
package cz.cvut.kbss.jopa.model.metamodel;

import cz.cvut.kbss.jopa.exception.MetamodelInitializationException;
import cz.cvut.kbss.jopa.model.EntityTypeImpl;
import cz.cvut.kbss.jopa.model.MetamodelImpl;
import org.junit.Test;
import org.mockito.Mock;

import java.lang.reflect.Field;

public class EntityFieldMetamodelProcessorTest {

    @Mock
    private EntityTypeImpl<InvalidClass> etMock;
    @Mock
    private MetamodelImpl metamodelMock;

    @Test(expected = MetamodelInitializationException.class)
    public void processingNonTransientFieldWithoutPropertyInfoThrowsException() throws Exception {
        final EntityFieldMetamodelProcessor<InvalidClass> processor = new EntityFieldMetamodelProcessor<>(
                InvalidClass.class, etMock, metamodelMock);
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

}