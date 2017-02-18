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
package cz.cvut.kbss.jopa.exception;

import cz.cvut.kbss.jopa.exceptions.OWLPersistenceException;
import cz.cvut.kbss.jopa.model.lifecycle.LifecycleEvent;

import java.lang.reflect.Method;

/**
 * Indicates an error when building application persistence metamodel.
 */
public class MetamodelInitializationException extends OWLPersistenceException {

    public MetamodelInitializationException(String message, Throwable cause) {
        super(message, cause);
    }

    public MetamodelInitializationException(String message) {
        super(message);
    }

    public MetamodelInitializationException(Throwable cause) {
        super(cause);
    }

    public static MetamodelInitializationException multipleListenersForSameLifecycleEvent(Class<?> type,
                                                                                          LifecycleEvent event) {
        return new MetamodelInitializationException("Type " + type.getName() +
                " has multiple lifecycle callback methods for the same lifecycle event " +
                event.getAnnotation().getName());
    }

    public static MetamodelInitializationException invalidArgumentsForLifecycleListener(Class<?> type,
                                                                                        Method listener) {
        return new MetamodelInitializationException(
                incorrectLifecycleListenerSignatureMessage(type, listener) + " It should not have any arguments.");
    }

    private static String incorrectLifecycleListenerSignatureMessage(Class<?> type, Method listener) {
        return "The callback method [" + listener.getName() + "] in type [" + type.getName() +
                "] has an incorrect signature.";
    }

    public static MetamodelInitializationException invalidReturnTypeForLifecycleListener(Class<?> type,
                                                                                         Method listener) {
        return new MetamodelInitializationException(
                incorrectLifecycleListenerSignatureMessage(type, listener) + " Its return type should be void.");
    }

    public static MetamodelInitializationException invalidLifecycleListenerModifier(Class<?> type, Method listener) {
        return new MetamodelInitializationException(
                incorrectLifecycleListenerSignatureMessage(type, listener) + " It should not be static or final.");
    }
}
