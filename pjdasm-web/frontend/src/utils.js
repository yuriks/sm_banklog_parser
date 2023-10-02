/**
 * @param {Number} x
 * @returns {string}
 */
export function toHex2(x) {
    return x.toString(16).toUpperCase().padStart(2, "0");
}

/**
 * @param {Number} x
 * @returns {string}
 */
export function toHex4(x) {
    return x.toString(16).toUpperCase().padStart(4, "0");
}