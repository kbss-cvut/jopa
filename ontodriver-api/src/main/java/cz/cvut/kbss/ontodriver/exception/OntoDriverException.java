/*
 * JOPA
 * Copyright (C) 2025 Czech Technical University in Prague
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
package cz.cvut.kbss.ontodriver.exception;

import java.io.Serializable;

/**
 * Describes a general purpose exception that can be thrown by the OntoDriver.
 * <p>
 * This exception is likely to wrap another more specific exception (the cause).
 */
public class OntoDriverException extends Exception implements Serializable {

    public OntoDriverException() {
    }

    public OntoDriverException(String message) {
        super(message);
    }

    public OntoDriverException(Throwable cause) {
        super(cause);
    }

    public OntoDriverException(String message, Throwable cause) {
        super(message, cause);
    }
}
