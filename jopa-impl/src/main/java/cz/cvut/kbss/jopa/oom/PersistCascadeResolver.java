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
package cz.cvut.kbss.jopa.oom;

import cz.cvut.kbss.jopa.model.annotations.CascadeType;
import cz.cvut.kbss.jopa.model.annotations.OWLObjectProperty;
import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.jopa.model.metamodel.FieldSpecification;
import cz.cvut.kbss.jopa.oom.exceptions.EntityDeconstructionException;
import cz.cvut.kbss.jopa.utils.EntityPropertiesUtils;

import java.lang.reflect.Field;
import java.net.URI;

class PersistCascadeResolver extends CascadeResolver {

    PersistCascadeResolver(ObjectOntologyMapperImpl mapper) {
        super(mapper);
    }

    @Override
    protected void resolveFieldCascading(FieldSpecification<?, ?> fieldSpec, Object fieldValue, URI context) {
        OWLObjectProperty ann = fieldSpec.getJavaField().getAnnotation(OWLObjectProperty.class);
        assert ann != null;

        for (CascadeType c : ann.cascade()) {
            if (c == CascadeType.ALL || c == CascadeType.PERSIST) {
                return;
            }
        }
        final EntityType<?> et = mapper.getEntityType(fieldValue.getClass());
        final Field idField = et.getIdentifier().getJavaField();
        if (!idField.isAccessible()) {
            idField.setAccessible(true);
        }
        try {
            final URI id = EntityPropertiesUtils.getPrimaryKey(fieldValue, et);
            mapper.registerPendingPersist(id, fieldValue, context);
        } catch (IllegalArgumentException e) {
            throw new EntityDeconstructionException("Unable to check field cascading.", e);
        }
    }
}
