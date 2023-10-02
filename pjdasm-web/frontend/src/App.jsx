import {AsmView} from "./AsmView";
import {createResource, createSignal, Show} from "solid-js";
import {toHex2} from "./utils";


async function fetchBank(bankNum) {
    const response = await fetch(`/api/bank/${toHex2(bankNum)}`);
    return await response.json();
}

function App() {
    const [bankNum, setBankNum] = createSignal(0x80);
    const [bank] = createResource(bankNum, fetchBank);

    return <Show when={!bank.loading} fallback="Loading...">
        <AsmView bankLines={bank().lines}/>
    </Show>;
}

export default App;
