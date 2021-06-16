/**
 * Copyright (C) 2020 Czech Technical University in Prague
 * <p>
 * This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public
 * License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later
 * version.
 * <p>
 * This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
 * warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License along with this program. If not, see
 * <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.jopa.sessions;

import cz.cvut.kbss.jopa.adapters.IndirectCollection;
import cz.cvut.kbss.jopa.exceptions.OWLPersistenceException;
import cz.cvut.kbss.jopa.model.annotations.Types;
import cz.cvut.kbss.jopa.model.metamodel.CollectionType;
import cz.cvut.kbss.jopa.model.metamodel.PluralAttribute;
import cz.cvut.kbss.jopa.utils.CollectionFactory;
import cz.cvut.kbss.jopa.utils.EntityPropertiesUtils;
import cz.cvut.kbss.jopa.utils.MetamodelUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.lang.reflect.Constructor;
import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.security.AccessController;
import java.security.PrivilegedActionException;
import java.util.*;

/**
 * Special class for cloning collections. Introduced because some Java collection have no no-argument constructor and
 * thus they must be cloned specially. NOTE: This class may be removed in case a better cloning mechanisms (namely
 * database mappings and copy policies) is introduced.
 */
class CollectionInstanceBuilder extends AbstractInstanceBuilder {

    private static final Logger LOG = LoggerFactory.getLogger(CollectionInstanceBuilder.class);

    private static final Class<?> singletonListClass = Collections.singletonList(null).getClass();
    private static final Class<?> singletonSetClass = Collections.singleton(null).getClass();
    private static final Class<?> arrayAsListClass = Arrays.asList(null, null).getClass();

    private static final Class<? extends List> DEFAULT_LIST_CLASS = ArrayList.class;
    private static final Class<? extends Set> DEFAULT_SET_CLASS = HashSet.class;

    CollectionInstanceBuilder(CloneBuilderImpl builder, UnitOfWorkImpl uow) {
        super(builder, uow);
    }

    /**
     * This method is the entry point for cloning the Java collections. It clones standard collections as well as
     * immutable collections and singleton collections. </p>
     * <p>
     * Currently supported are List and Set.
     *
     * @param collection The collection to clone
     * @return A deep clone of the specified collection
     */
    @Override
    Object buildClone(Object cloneOwner, Field field, Object collection, CloneConfiguration configuration) {
        assert collection instanceof Collection;
        Collection<?> container = (Collection<?>) collection;
        if (container instanceof IndirectCollection<?>) {
            container = (Collection<?>) ((IndirectCollection<?>) container).unwrap();
        }
        if (Collections.emptyList() == container) {
            return Collections.emptyList();
        }
        if (Collections.emptySet() == container) {
            return Collections.emptySet();
        }
        Object clone = cloneUsingDefaultConstructor(cloneOwner, field, container, configuration);
        if (clone == null) {
            clone = buildInstanceOfSpecialCollection(cloneOwner, field, container, configuration);
        }
        if (clone == null) {
            clone = buildDefaultCollectionInstance(cloneOwner, field, container, configuration);
        }
        clone = uow.createIndirectCollection(clone, cloneOwner, field);
        return clone;
    }

    /**
     * Clones the specified collection using its default zero argument constructor. If the specified collection has none
     * (e. g. like SingletonList), this method returns null.
     *
     * @param container The collection to clone.
     * @return cloned collection
     */
    private Collection<?> cloneUsingDefaultConstructor(Object cloneOwner, Field field,
                                                       Collection<?> container, CloneConfiguration configuration) {
        Class<?> javaClass = container.getClass();
        final Optional<Collection<?>> result = createNewInstance(javaClass, container.size());
        // Makes shallow copy
        result.ifPresent(r -> cloneCollectionContent(cloneOwner, field, container, r, configuration));
        return result.orElse(null);
    }

    private static Optional<Collection<?>> createNewInstance(Class<?> type, int size) {
        Object[] params = null;
        Class<?>[] types = {int.class};
        // Look for constructor taking initial size as parameter
        Constructor<?> ctor = getDeclaredConstructorFor(type, types);
        if (ctor != null) {
            params = new Object[1];
            params[0] = size;
        } else {
            ctor = DefaultInstanceBuilder.getDeclaredConstructorFor(type, null);
        }
        if (ctor == null) {
            return Optional.empty();
        }
        Collection<?> result = null;
        try {
            result = (Collection<?>) ctor.newInstance(params);
        } catch (InstantiationException | InvocationTargetException | IllegalArgumentException e) {
            throw new OWLPersistenceException(e);
        } catch (IllegalAccessException e) {
            logConstructorAccessException(ctor, e);
            try {
                result = (Collection<?>) AccessController.doPrivileged(new PrivilegedInstanceCreator(ctor));
            } catch (PrivilegedActionException ex) {
                logPrivilegedConstructorAccessException(ctor, ex);
                // Do nothing
            }
        }
        return Optional.ofNullable(result);
    }

    /**
     * Clone all the elements in the collection. This will make sure that the cloning process creates a deep copy.
     *
     * @param source The collection to clone.
     */
    private void cloneCollectionContent(Object cloneOwner, Field field, Collection<?> source,
                                        Collection<?> target, CloneConfiguration configuration) {
        if (source.isEmpty()) {
            return;
        }
        Collection<Object> tg = (Collection<Object>) target;
        for (Object elem : source) {
            if (elem == null) {
                tg.add(null);
                continue;
            }
            if (CloneBuilderImpl.isImmutable(elem)) {
                tg.addAll(source);
                break;
            }
            tg.add(cloneCollectionElement(cloneOwner, field, elem, configuration));
        }
    }

    private Object cloneCollectionElement(Object cloneOwner, Field field, Object element,
                                          CloneConfiguration configuration) {
        Object clone;
        if (builder.isTypeManaged(element.getClass())) {
            clone = uow.registerExistingObject(element, configuration.getDescriptor(), configuration.getPostRegister());
        } else {
            clone = builder.buildClone(cloneOwner, field, element, configuration.getDescriptor());
        }
        return clone;
    }


    private Collection<?> buildInstanceOfSpecialCollection(Object cloneOwner, Field field, Collection<?> container,
                                                           CloneConfiguration configuration) {
        if (arrayAsListClass.isInstance(container)) {
            final List<?> arrayList = new ArrayList<>(container.size());
            cloneCollectionContent(cloneOwner, field, container, arrayList, configuration);
            return arrayList;
        } else if (singletonListClass.isInstance(container) || singletonSetClass.isInstance(container)) {
            final Object element = container.iterator().next();
            final Object elementClone = CloneBuilderImpl.isImmutable(element) ? element :
                                        cloneCollectionElement(cloneOwner, field, element, configuration);
            return singletonListClass.isInstance(container) ? Collections.singletonList(elementClone) :
                   Collections.singleton(elementClone);
        } else {
            return null;
        }
    }

    private Collection<?> buildDefaultCollectionInstance(Object cloneOwner, Field field, Collection<?> container,
                                                         CloneConfiguration configuration) {
        LOG.trace("Unable to find matching collection constructor. Creating default collection.");
        final Collection<?> clone;
        try {
            if (container instanceof List) {
                clone = DEFAULT_LIST_CLASS.newInstance();
            } else if (container instanceof Set) {
                clone = DEFAULT_SET_CLASS.newInstance();
            } else {
                throw new OWLPersistenceException(
                        "Cannot clone unsupported collection instance of type " + container.getClass() + ".");
            }
            cloneCollectionContent(cloneOwner, field, container, clone, configuration);
        } catch (InstantiationException | IllegalAccessException e) {
            throw new OWLPersistenceException(e);
        }
        return clone;
    }

    @Override
    void mergeChanges(Field field, Object target, Object originalValue, Object cloneValue) {
        assert originalValue == null || originalValue instanceof Collection;
        assert cloneValue instanceof Collection;

        Collection<Object> clone = (Collection<Object>) cloneValue;
        if (clone instanceof IndirectCollection) {
            clone = ((IndirectCollection<Collection<Object>>) clone).unwrap();
        }
        final Optional<Collection<?>> origOpt = createNewInstance(clone.getClass(), clone.size());
        Collection<Object> orig = (Collection<Object>) origOpt.orElse(createDefaultCollection(clone.getClass()));
        EntityPropertiesUtils.setFieldValue(field, target, orig);

        if (clone.isEmpty()) {
            return;
        }
        for (Object cl : clone) {
            orig.add(uow.contains(cl) ? builder.getOriginal(cl) : cl);
        }
        final Types types = field.getAnnotation(Types.class);
        if (types != null) {
            MetamodelUtils.checkForModuleSignatureExtension(orig, builder.getMetamodel());
        }
    }

    private static Collection<Object> createDefaultCollection(Class<?> cls) {
        return CollectionFactory.createDefaultCollection(CollectionType.fromClass(cls));
    }

    @Override
    boolean populatesAttributes() {
        return true;
    }
}
