/**
 */
package eu.fittest.test.project;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import org.eclipse.emf.common.util.AbstractEnumerator;

/**
 * <!-- begin-user-doc -->
 * A representation of the literals of the enumeration '<em><b>SUT Technology Type</b></em>',
 * and utility methods for working with them.
 * <!-- end-user-doc -->
 * @see eu.fittest.test.project.ProjectPackage#getSUTTechnologyType()
 * @model extendedMetaData="name='SUTTechnologyType'"
 * @generated
 */
public final class SUTTechnologyType extends AbstractEnumerator {
	/**
	 * The '<em><b>FLASH</b></em>' literal value.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of '<em><b>FLASH</b></em>' literal object isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @see #FLASH_LITERAL
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int FLASH = 0;

	/**
	 * The '<em><b>PHP</b></em>' literal value.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of '<em><b>PHP</b></em>' literal object isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @see #PHP_LITERAL
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int PHP = 1;

	/**
	 * The '<em><b>HTML</b></em>' literal value.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of '<em><b>HTML</b></em>' literal object isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @see #HTML_LITERAL
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int HTML = 2;

	/**
	 * The '<em><b>FLASH</b></em>' literal object.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #FLASH
	 * @generated
	 * @ordered
	 */
	public static final SUTTechnologyType FLASH_LITERAL = new SUTTechnologyType(FLASH, "FLASH", "FLASH");

	/**
	 * The '<em><b>PHP</b></em>' literal object.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #PHP
	 * @generated
	 * @ordered
	 */
	public static final SUTTechnologyType PHP_LITERAL = new SUTTechnologyType(PHP, "PHP", "PHP");

	/**
	 * The '<em><b>HTML</b></em>' literal object.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #HTML
	 * @generated
	 * @ordered
	 */
	public static final SUTTechnologyType HTML_LITERAL = new SUTTechnologyType(HTML, "HTML", "HTML");

	/**
	 * An array of all the '<em><b>SUT Technology Type</b></em>' enumerators.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private static final SUTTechnologyType[] VALUES_ARRAY =
		new SUTTechnologyType[] {
			FLASH_LITERAL,
			PHP_LITERAL,
			HTML_LITERAL,
		};

	/**
	 * A public read-only list of all the '<em><b>SUT Technology Type</b></em>' enumerators.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public static final List VALUES = Collections.unmodifiableList(Arrays.asList(VALUES_ARRAY));

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
			case FLASH: return FLASH_LITERAL;
			case PHP: return PHP_LITERAL;
			case HTML: return HTML_LITERAL;
		}
		return null;
	}

	/**
	 * Only this class can construct instances.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private SUTTechnologyType(int value, String name, String literal) {
		super(value, name, literal);
	}

} //SUTTechnologyType
