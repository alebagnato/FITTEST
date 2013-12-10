/**
 */
package eu.fittest.test.project;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import org.eclipse.emf.common.util.Enumerator;

/**
 * <!-- begin-user-doc -->
 * A representation of the literals of the enumeration '<em><b>SUT Technology Type</b></em>',
 * and utility methods for working with them.
 * <!-- end-user-doc -->
 * @see eu.fittest.test.project.ProjectPackage#getSUTTechnologyType()
 * @model extendedMetaData="name='SUTTechnologyType'"
 * @generated
 */
public enum SUTTechnologyType implements Enumerator {
	/**
	 * The '<em><b>FLASH</b></em>' literal object.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #FLASH_VALUE
	 * @generated
	 * @ordered
	 */
	FLASH(0, "FLASH", "FLASH"),

	/**
	 * The '<em><b>PHP</b></em>' literal object.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #PHP_VALUE
	 * @generated
	 * @ordered
	 */
	PHP(1, "PHP", "PHP"),

	/**
	 * The '<em><b>HTML</b></em>' literal object.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #HTML_VALUE
	 * @generated
	 * @ordered
	 */
	HTML(2, "HTML", "HTML");

	/**
	 * The '<em><b>FLASH</b></em>' literal value.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of '<em><b>FLASH</b></em>' literal object isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @see #FLASH
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int FLASH_VALUE = 0;

	/**
	 * The '<em><b>PHP</b></em>' literal value.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of '<em><b>PHP</b></em>' literal object isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @see #PHP
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int PHP_VALUE = 1;

	/**
	 * The '<em><b>HTML</b></em>' literal value.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of '<em><b>HTML</b></em>' literal object isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @see #HTML
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int HTML_VALUE = 2;

	/**
	 * An array of all the '<em><b>SUT Technology Type</b></em>' enumerators.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private static final SUTTechnologyType[] VALUES_ARRAY =
		new SUTTechnologyType[] {
			FLASH,
			PHP,
			HTML,
		};

	/**
	 * A public read-only list of all the '<em><b>SUT Technology Type</b></em>' enumerators.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public static final List<SUTTechnologyType> VALUES = Collections.unmodifiableList(Arrays.asList(VALUES_ARRAY));

	/**
	 * Returns the '<em><b>SUT Technology Type</b></em>' literal with the specified literal value.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public static SUTTechnologyType get(String literal) {
		for (int i = 0; i < VALUES_ARRAY.length; ++i) {
			SUTTechnologyType result = VALUES_ARRAY[i];
			if (result.toString().equals(literal)) {
				return result;
			}
		}
		return null;
	}

	/**
	 * Returns the '<em><b>SUT Technology Type</b></em>' literal with the specified name.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public static SUTTechnologyType getByName(String name) {
		for (int i = 0; i < VALUES_ARRAY.length; ++i) {
			SUTTechnologyType result = VALUES_ARRAY[i];
			if (result.getName().equals(name)) {
				return result;
			}
		}
		return null;
	}

	/**
	 * Returns the '<em><b>SUT Technology Type</b></em>' literal with the specified integer value.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public static SUTTechnologyType get(int value) {
		switch (value) {
			case FLASH_VALUE: return FLASH;
			case PHP_VALUE: return PHP;
			case HTML_VALUE: return HTML;
		}
		return null;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private final int value;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private final String name;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private final String literal;

	/**
	 * Only this class can construct instances.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private SUTTechnologyType(int value, String name, String literal) {
		this.value = value;
		this.name = name;
		this.literal = literal;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public int getValue() {
	  return value;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public String getName() {
	  return name;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public String getLiteral() {
	  return literal;
	}

	/**
	 * Returns the literal value of the enumerator, which is its string representation.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public String toString() {
		return literal;
	}
	
} //SUTTechnologyType
