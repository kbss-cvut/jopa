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
package cz.cvut.kbss.jopa.model.metamodel;

import cz.cvut.kbss.jopa.exception.MetamodelInitializationException;
import cz.cvut.kbss.jopa.model.annotations.EntityListeners;
import cz.cvut.kbss.jopa.model.lifecycle.LifecycleEvent;

import java.lang.reflect.Method;
import java.lang.reflect.Modifier;

class EntityLifecycleCallbackResolver {

    private final AbstractIdentifiableType<?> managedType;
    private final EntityLifecycleListenerManager manager;

    /**
     * Creates a lifecycle callback resolver for the specified entity type.
     * @param et The type to process
     */
    EntityLifecycleCallbackResolver(AbstractIdentifiableType<?> et) {
        this.managedType = et;
        this.manager = new EntityLifecycleListenerManager();
    }

    /**
     * Builds an instance of {@link EntityLifecycleListenerManager} for the specified managed type.
     * <p>
     * The manager contains:
     * <ul>
     * <li>Lifecycle callbacks declared in the entity class,</li>
     * <li>EntityListener instance declared on the entity class,</li>
     * <li>Reference to parent {@link EntityLifecycleListenerManager} (if exists).</li>
     * </ul>
     *
     * @return Lifecycle listener manager instance
     */
    EntityLifecycleListenerManager resolve() {
        resolveLifecycleCallbacks();
        resolveEntityListeners();
        if (managedType.getSupertype() != null) {
            manager.setParent(managedType.getSupertype().getLifecycleListenerManager());
        }
        return manager;
    }

    private void resolveLifecycleCallbacks() {
        final Class<?> cls = managedType.getJavaType();
        for (Method m : cls.getDeclaredMethods()) {
            for (LifecycleEvent hookType : LifecycleEvent.values()) {
                if (m.getDeclaredAnnotation(hookType.getAnnotation()) != null) {
                    verifyCallbackNotAlreadyDefined(hookType);
                    verifyLifecycleCallbackSignature(m);
                    manager.addLifecycleCallback(hookType, m);
                }
            }
        }
    }

    private void verifyCallbackNotAlreadyDefined(LifecycleEvent hookType) {
        if (manager.hasLifecycleCallback(hookType)) {
            throw new MetamodelInitializationException("The type [" + managedType.getJavaType().getName() +
                    "] has multiple lifecycle callbacks for the lifecycle event [" + hookType + "].");
        }
    }

    private void verifyLifecycleCallbackSignature(Method callback) {
        if (callback.getParameterCount() > 0) {
            throw MetamodelInitializationException
                    .invalidArgumentsForLifecycleListener(managedType.getJavaType(), callback);
        }
        if (!callback.getReturnType().equals(Void.TYPE)) {
            throw MetamodelInitializationException
                    .invalidReturnTypeForLifecycleListener(managedType.getJavaType(), callback);
        }
        if (Modifier.isFinal(callback.getModifiers()) || Modifier.isStatic(callback.getModifiers())) {
            throw MetamodelInitializationException
                    .invalidLifecycleListenerModifier(managedType.getJavaType(), callback);
        }
    }

    private void resolveEntityListeners() {
        final EntityListeners listenersAnn = managedType.getJavaType().getDeclaredAnnotation(EntityListeners.class);
        if (listenersAnn == null) {
            return;
        }
        for (Class<?> listenerType : listenersAnn.value()) {
            try {
                final Object listener = listenerType.newInstance();
                manager.addEntityListener(listener);
                resolveEntityListenerCallbacks(listener, listenerType);
            } catch (InstantiationException | IllegalAccessException e) {
                throw new MetamodelInitializationException("Unable to instantiate entity listener of type "
                        + listenerType + ". The listener has to have a public no-arg constructor.");
            }
        }
    }

    private void resolveEntityListenerCallbacks(Object listener, Class<?> listenerType) {
        for (Method m : listenerType.getDeclaredMethods()) {
            for (LifecycleEvent hookType : LifecycleEvent.values()) {
                if (m.getDeclaredAnnotation(hookType.getAnnotation()) != null) {
                    verifyEntityListenerCallbackNotAlreadyDefined(listener, listenerType, hookType);
                    verifyEntityListenerCallbackSignature(listenerType, m);
                    manager.addEntityListenerCallback(listener, hookType, m);
                }
            }
        }
    }

    private void verifyEntityListenerCallbackNotAlreadyDefined(Object listener, Class<?> listenerType,
                                                               LifecycleEvent event) {
        if (manager.hasEntityListenerCallback(listener, event)) {
            throw new MetamodelInitializationException("The entity listener [" + listenerType.getName() +
                    "] has multiple callbacks for the lifecycle event [" + event + "].");
        }
    }

    private void verifyEntityListenerCallbackSignature(Class<?> listenerType, Method callback) {
        verifyCallbackParameterCount(listenerType, callback);
        verifyCallbackParameterTypes(listenerType, callback);
        verifyCallbackReturnType(listenerType, callback);
        verifyCallbackModifiers(listenerType, callback);
    }

    private void verifyCallbackModifiers(Class<?> listenerType, Method callback) {
        if (Modifier.isFinal(callback.getModifiers()) || Modifier.isStatic(callback.getModifiers())) {
            throw MetamodelInitializationException.invalidEntityListenerCallbackModifier(listenerType, callback);
        }
    }

    private void verifyCallbackReturnType(Class<?> listenerType, Method callback) {
        if (!callback.getReturnType().equals(Void.TYPE)) {
            throw MetamodelInitializationException.invalidReturnTypeForEntityListenerCallback(listenerType, callback);
        }
    }

    private void verifyCallbackParameterTypes(Class<?> listenerType, Method callback) {
        final Class<?> paramType = callback.getParameterTypes()[0];
        if (!paramType.isAssignableFrom(Object.class) && !paramType.isAssignableFrom(managedType.getJavaType())) {
            throw MetamodelInitializationException
                    .invalidEntityListenerCallbackParameterType(managedType.getJavaType(), listenerType, callback);
        }
    }

    private void verifyCallbackParameterCount(Class<?> listenerType, Method callback) {
        if (callback.getParameterCount() != 1) {
            throw MetamodelInitializationException
                    .invalidArgumentsForEntityListenerCallback(listenerType, callback);
        }
    }
}
