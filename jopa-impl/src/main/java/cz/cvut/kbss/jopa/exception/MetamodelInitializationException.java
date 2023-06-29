/**
 * Copyright (C) 2023 Czech Technical University in Prague
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
package cz.cvut.kbss.jopa.exception;

import cz.cvut.kbss.jopa.exceptions.OWLPersistenceException;

import java.lang.reflect.Method;

/**
 * Indicates an error when building application persistence metamodel.
 */
public class MetamodelInitializationException extends OWLPersistenceException {

    private static final String VOID_RETURN_TYPE_MSG = " Its return type should be void.";
    private static final String INVALID_MODIFIER_MSG = " It should not be static or final.";

    public MetamodelInitializationException(String message, Throwable cause) {
        super(message, cause);
    }

    public MetamodelInitializationException(String message) {
        super(message);
    }

    public MetamodelInitializationException(Throwable cause) {
        super(cause);
    }

    public static MetamodelInitializationException invalidArgumentsForLifecycleListener(Class<?> type,
                                                                                        Method listener) {
        return new MetamodelInitializationException(
                incorrectLifecycleListenerSignatureMessage(type, listener) + " It should not have any arguments.");
    }

    private static String incorrectLifecycleListenerSignatureMessage(Class<?> type, Method listener) {
        return "The callback method [" + listener.getName() + "] in type [" + type.getName() +
                "] has incorrect signature.";
    }

    public static MetamodelInitializationException invalidReturnTypeForLifecycleListener(Class<?> type,
                                                                                         Method listener) {
        return new MetamodelInitializationException(
                incorrectLifecycleListenerSignatureMessage(type, listener) + VOID_RETURN_TYPE_MSG);
    }

    public static MetamodelInitializationException invalidLifecycleListenerModifier(Class<?> type, Method listener) {
        return new MetamodelInitializationException(
                incorrectLifecycleListenerSignatureMessage(type, listener) + INVALID_MODIFIER_MSG);
    }

    public static MetamodelInitializationException invalidArgumentsForEntityListenerCallback(Class<?> type,
                                                                                             Method callback) {
        return new MetamodelInitializationException(incorrectEntityListenerCallbackSignatureMessage(type, callback) +
                " It should take exactly one argument.");
    }

    private static String incorrectEntityListenerCallbackSignatureMessage(Class<?> type, Method callback) {
        return "The callback method [" + callback.getName() + "] in entity listener [" + type.getName() +
                "] has incorrect signature.";
    }

    public static MetamodelInitializationException invalidReturnTypeForEntityListenerCallback(Class<?> type,
                                                                                              Method callback) {
        return new MetamodelInitializationException(
                incorrectEntityListenerCallbackSignatureMessage(type, callback) + VOID_RETURN_TYPE_MSG);
    }

    public static MetamodelInitializationException invalidEntityListenerCallbackModifier(Class<?> type,
                                                                                         Method callback) {
        return new MetamodelInitializationException(
                incorrectEntityListenerCallbackSignatureMessage(type, callback) + INVALID_MODIFIER_MSG);
    }

    public static MetamodelInitializationException invalidEntityListenerCallbackParameterType(Class<?> managedType,
                                                                                              Class<?> type,
                                                                                              Method callback) {
        return new MetamodelInitializationException(
                incorrectEntityListenerCallbackSignatureMessage(type, callback) + " Its parameter should be of type [" +
                        Object.class.getName() + "] or [" + managedType.getName() + "].");
    }
}
