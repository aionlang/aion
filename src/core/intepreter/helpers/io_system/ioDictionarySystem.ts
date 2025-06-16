import {IOSystem} from "./ioSystem";

/**
 * Uses a dictionary for storing the data
 */
export class IODictionarySystem implements IOSystem {

    private existingCalendarData: Map<string, string>;

    constructor(existingCalendars: Map<string, string> ) {
        this.existingCalendarData = existingCalendars;
    }

    importFile(path: string): string {
        if (!this.existingCalendarData.has(path)) {
            throw new Error(`File not found: ${path}`);
        }
        return this.existingCalendarData.get(path);
    }

    saveFile(fileName: string, fileContents: string): boolean {
        this.existingCalendarData.set(fileName, fileContents);
        return true; // return something meaningful;
    }
}

