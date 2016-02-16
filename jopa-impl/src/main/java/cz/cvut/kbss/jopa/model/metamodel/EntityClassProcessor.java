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
import cz.cvut.kbss.jopa.model.IRI;
import cz.cvut.kbss.jopa.model.annotations.OWLClass;

public class EntityClassProcessor {

    public <T> EntityTypeImpl<T> processEntityType(Class<T> cls) {
        final OWLClass c = cls.getAnnotation(OWLClass.class);

        if (c == null) {
            throw new MetamodelInitializationException("The class " + cls + " is not an OWLPersistence entity!");
        }

        checkForNoArgConstructor(cls);

        return new EntityTypeImpl<>(cls.getSimpleName(), cls, IRI.create(c.iri()));
    }

    private <T> void checkForNoArgConstructor(Class<T> cls) {
        try {
            cls.getDeclaredConstructor();
        } catch (NoSuchMethodException e) {
            throw new MetamodelInitializationException("Class " + cls + " is missing required no-arg constructor.");
        }
    }
}
