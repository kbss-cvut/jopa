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
package cz.cvut.kbss.ontodriver.owlapi.exception;

import cz.cvut.kbss.ontodriver.exception.OntoDriverException;

/**
 * General exception for the OWLAPI driver.
 */
public class OwlapiDriverException extends OntoDriverException {

    public OwlapiDriverException() {
    }

    public OwlapiDriverException(String message) {
        super(message);
    }

    public OwlapiDriverException(Throwable cause) {
        super(cause);
    }

    public OwlapiDriverException(String message, Throwable cause) {
        super(message, cause);
    }
}
